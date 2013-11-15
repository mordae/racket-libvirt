#lang racket/base
;
; Libvirt Client
;

(require racket/contract
         racket/generator
         racket/function
         racket/match
         racket/class
         racket/dict
         tandem
         throw
         xdr)

(require "private/common.rkt"
         "private/protocol.rkt"
         "private/util.rkt")

(provide libvirt%
         exn:fail:libvirt?
         exn:fail:libvirt:error?
         exn:fail:libvirt:connection?)


;; Our custom exception type.
(define-struct (exn:fail:libvirt exn:fail)
  ())

;; Exception for remote procedure call errors.
(define-struct (exn:fail:libvirt:error exn:fail:libvirt)
  ())

;; Exception for network problems.
(define-struct (exn:fail:libvirt:connection exn:fail:libvirt)
  ())


(define (write-failed (exn #f))
  (throw exn:fail:libvirt:connection
         'libvirt "server closed our connection during write"))


(define (read-failed (exn #f))
  (throw exn:fail:libvirt:connection
         'libvirt "server closed our connection during read"))


;; Read bytes from given input port or throw an exception if
;; given amount could not be read or end of file have been reached.
(define/contract (read-bytes/safe amt in)
                 (-> exact-nonnegative-integer? input-port? bytes?)
  (with-handlers ((exn:fail? read-failed))
    (let ((bstr (read-bytes amt in)))
      (when (or (eof-object? bstr) (< (bytes-length bstr) amt))
        (read-failed))
      bstr)))


;; Write bytes to given port or throw an exception if
;; given amount could not be written.
(define/contract (write-bytes/safe bstr out)
                 (-> bytes? output-port? exact-nonnegative-integer?)
  (with-handlers ((exn:fail? write-failed))
    (let ((amt (write-bytes bstr out)))
      (when (> (bytes-length bstr) amt)
        (write-failed))
      amt)))


;; Flush output port or throw an exception if port is closed.
(define/contract (flush-output/safe out)
                 (-> output-port? void?)
  (with-handlers ((exn:fail? write-failed))
    (flush-output out)))


;; Object encapsulating wire connection details such as framing and
;; request/reply matching.  Built around a tandem object.
(define libvirt-connection%
  (class object%
    ;; Input and output ports for communication with the server.
    (init-field in out)

    ;; Tandem object takes care of request/reply matching and
    ;; distributes work among interested threads without separate
    ;; background worker.
    (field (work (tandem (lambda (tag value)
                           (transmit tag value))
                         (lambda ()
                           (receive)))))


    ;; Transmit a single procedure call.
    (define/private (transmit serial args)
      (match args
        ((list type method args ...)
         ;; Create new message header with specified serial number.
         (define header
           (vector remote-program remote-version method type serial 'ok))

         ;; Lookup information about the remote procedure.
         (define info (dict-ref remote-calls method))

         ;; Serialize both message header and the body in one packet.
         (define packet
           (bytes-append (dump/bytes message-header header)
                         (dump/bytes (call-info-args info)
                                     (list->vector args))))

         ;; Send packet length and the packet itself.
         (write-bytes/safe
           (integer->integer-bytes (+ 4 (bytes-length packet)) 4 #t #t) out)
         (write-bytes/safe packet out)
         (flush-output/safe out))))


    ;; Receive a single message.
    (define/private (receive)
      (let* ((len    (integer-bytes->integer (read-bytes/safe 4 in) #t #t))
             (header (load/bytes message-header (read-bytes/safe 24 in)))
             (body   (read-bytes/safe (- len 28) in)))
        (let-values (((prog vers method type serial status)
                      (apply values (vector->list header))))
          (match type
            ('reply
             (let* ((type   (call-info-ret (dict-ref remote-calls method)))
                    (serial (vector-ref header 4))
                    (status (vector-ref header 5))
                    (value  (match status
                              ('ok    (load/bytes type body))
                              ('error (load/bytes remote-error body)))))
                 (values serial (cons status value))))

            (else
             (printf "unexpected ~a (~a ~a ~a)\n" type method serial status)
             (receive))))))


    ;; Infinite sequence of serial numbers for messages.
    (define next-serial
      (synchronized
        (generator ()
          (for ((i (in-naturals 1)))
            (yield i)))))


    ;; Perform remote method call.
    (define/public (remote-call method . args)
      (let ((serial (next-serial)))
        (match (tandem-call work serial (list* 'call method args))
          ((cons status value)
           (match status
             ('ok    (vector->values value))
             ('error (throw exn:fail:libvirt:error
                            method "remote procedure call failed"
                                   "reason" (vector-ref value 2))))))))


    ;; Initialize the parent class.
    (super-new)))


;; Libvirt RPC client.
(define libvirt%
  (class object%
    ;; Input and output ports for communication with the server.
    (init-field in out)

    ;; Connection is realized in a separate class.
    (field (connection
             (new libvirt-connection% (in in) (out out))))


    ;; Forward remote call to the connection instance.
    (define/private (remote-call method . args)
      (apply dynamic-send connection 'remote-call method args))


    ;; Attach to hypervisor management.
    (define/public (open (target "qemu:///system"))
      (remote-call 'connect-open (->bstr target) 0))


    ;; High-level hypervisor information.
    (define/public (node-info)
      (let-values (((model memory cpus mhz nodes sockets cores threads)
                    (remote-call 'node-get-info)))
        (hasheq 'model   (->str (list->bytes model))
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
        (->xexpr (->str sysinfo))))


    ;; Initialize the parent class.
    (super-new)))


; vim:set ts=2 sw=2 et:
