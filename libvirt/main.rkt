#lang racket/base
;
; Libvirt Client
;

(require racket/contract
         racket/function
         racket/class
         racket/dict
         xdr)

(require "private/protocol.rkt"
         "private/util.rkt")

(provide (all-defined-out))


;; Our custom exception type.
(define-struct (exn:fail:libvirt exn:fail)
  ())

;; Exception for remote procedure call errors.
(define-struct (exn:fail:libvirt:error exn:fail:libvirt)
  (details))

;; Exception for network problems.
(define-struct (exn:fail:libvirt:connection exn:fail:libvirt)
  ())


;; Libvirt RPC client.
(define libvirt%
  (class object%
    ;; Input and output ports for communication with the server.
    (init-field in out)


    ;; Serial number of the last RPC call.
    (field (serial 0))


    ;; Table of pending RPC calls, keyed by their serial numbers.
    (field (pending-calls (make-hash)))


    ;; Raises a libvirt exception using specified error vector.
    (define (libvirt-error err)
      (raise (exn:fail:libvirt:error (->s (vector-ref err 2))
                                     (current-continuation-marks)
                                     err)))


    ;; Create remote call header.
    (define (remote-call-header method)
      (set! serial (add1 serial))
      (vector remote-program remote-version method 'call serial 'ok))


    ;; Perform remote method call.
    (define (remote-call method . args)
      ;; Create new message header with unique serial number.
      (define header (remote-call-header method))

      ;; Lookup information about the remote call arguments and return values.
      (define info (dict-ref remote-calls method))

      ;; Serialize the outgoing message body as procedure arguments.
      (define body (dump/bytes (call-info-args info) (list->vector args)))

      ;; Serialize message header.
      (define head (dump/bytes message-header header))

      ;; Create the outgoing packet by combining head and body.
      (define packet (bytes-append head body))

      ;; Prepare channel for response.
      (define response-channel (make-channel))

      ;; Register asynchronous callback that will receive the reply.
      (hash-set! pending-calls (vector-ref header 4)
                 (lambda args
                   (channel-put response-channel args)))


      ;; Write the outgoing packet length and the packet itself.
      (write-bytes
        (integer->integer-bytes (+ 4 (bytes-length packet)) 4 #t #t) out)
      (write-bytes packet out)
      (flush-output out)

      ;; Wait for the reply.
      (let ((result (sync (get-field reader this) response-channel)))
        (cond
          ;; The background thread might have died in the meantime.
          ;; We need to inform the caller.
          ((thread? result)
           (raise (exn:fail:libvirt:connection
                    "libvirtd closed the connection"
                    (current-continuation-marks) #f)))

          (else
           ;; In the more probable case of our success, process the result.
           (let-values (((status body) (apply values result)))
             ;; Remove the pending call.
             (hash-remove! pending-calls (vector-ref header 4))

             (if (eq? status 'error)
               ;; Convert error response to an exception.
               (libvirt-error (load/bytes remote-error body))

               ;; Decode successfull response as method return type.
               (apply values (vector->list
                               (load/bytes (call-info-ret info) body)))))))))


    ;; Attach to hypervisor management.
    (define/public (open (target "qemu:///system"))
      (let-values ((() (remote-call 'connect-open (->b target) 0)))
        (void)))


    ;; High-level hypervisor information.
    (define/public (node-info)
      (let-values (((model memory cpus mhz nodes sockets cores threads)
                    (remote-call 'node-get-info)))
        (hasheq 'model   (->s (list->bytes model))
                'memory  memory
                'cpus    cpus
                'mhz     mhz
                'nodes   nodes
                'sockets sockets
                'cores   cores
                'threads threads)))


    ;; Returns xexpr with system info.
    (define/public (system-info)
      (let-values (((sysinfo) (remote-call 'connect-get-sysinfo 0)))
        (->x (->s sysinfo))))


    ;; Incoming message reader.
    (define (reader-main)
      (let* ((len    (integer-bytes->integer (read-bytes 4 in) #t #t))
             (header (load/bytes message-header (read-bytes 24 in)))
             (body   (read-bytes (- len 28) in)))
        (let-values (((prog vers method type serial status)
                      (apply values (vector->list header))))
          (cond
            ((eq? type 'reply)
             ((hash-ref pending-calls serial) status body))

            (else
             (printf "unexpected ~a (~a ~a ~a)\n"
                     type method serial status)))))
      (reader-main))


    ;; Create background thread that will read messages from libvirt
    ;; and deliver them to pending calls or callbacks.
    (field (reader (thread reader-main)))


    ;; Suspend the background thread and forget both ports.
    ;; It is an error to call anything else after closing the client.
    (define/public (close)
      (remote-call 'connect-close)
      (thread-suspend reader)
      (set! in #f)
      (set! out #f))


    ;; Initialize the parent class.
    (super-new)))


; vim:set ts=2 sw=2 et:
