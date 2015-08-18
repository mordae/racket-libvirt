#lang racket/base
;
; Libvirt Wire Protocol
;

(require racket/contract
         racket/generator
         racket/unix-socket
         racket/format
         racket/match
         racket/tcp)

(require misc1/syntax
         misc1/throw
         misc1/async
         tandem
         xdr)

(require "types.rkt")

(provide
  (contract-out
    (libvirt-connection? predicate/c)

    (current-libvirt-connection
      (parameter/c libvirt-connection?))

    (make-libvirt-connection
      (-> input-port? output-port? libvirt-connection?))

    (libvirt-connect/unix
      (->* () (unix-socket-path?) libvirt-connection?))

    (libvirt-connect/tcp
      (->* () (string? port/c string? port/c) libvirt-connection?))))

(provide define-caller
         define-callers)


(define port/c
  (integer-in 1 65535))


(struct libvirt-connection
  (tandem))

(struct packet
  (type procno status payload)
  #:transparent)


(define current-libvirt-connection
  (make-parameter #f))


;; Connection with user-defined input and output ports.
(define (make-libvirt-connection in out)
  (let ((lock (make-semaphore 1)))
    (libvirt-connection (libvirt-tandem in out))))


;; Shortcut for tcp-based connections, defaulting to local libvirtd.
(define (libvirt-connect/tcp (host "127.0.0.1")
                             (port 16509)
                             (local-host #f)
                             (local-port #f))
  (let-values (((in out) (tcp-connect host port local-host local-port)))
    (make-libvirt-connection in out)))


;; Shortcut for domain socket connections, defaulting to local libvirtd.
(define (libvirt-connect/unix (path "/var/run/libvirt/libvirt-sock"))
  (let-values (((in out) (unix-socket-connect path)))
    (make-libvirt-connection in out)))


;; Create tandem for those streams.
(define (libvirt-tandem in out)
  (let ((wlock (make-semaphore 1)))
    (tandem (async/loop
              (read-packet in))
            (λ (tag value)
              (with-semaphore wlock
                (write-packet tag value out))))))


;; Consume a packet from the input stream, using the libvirt's
;; framing protocol of 4-byte prefix followed by the data,
;; where length specified in the prefix includes itself.
(define (read-packet in)
  (let ((length (- (load uint in) 4)))
    (decode-packet
      (read-bytes length in))))


;; Decode byte string to a packet structure.
(define (decode-packet bstr)
  (match (load/bytes message-header bstr)
    ((vector (and program (not REMOTE-PROGRAM)) _ _ _ _ _)
     (throw exn:fail 'libvirt "unrecognized peer program"
                     "expected" (~s REMOTE-PROGRAM)
                     "received" (~s program)))

    ((vector _ (and version (not REMOTE-VERSION)) _ _ _ _)
     (throw exn:fail 'libvirt "protocol version mismatch"
                     "expected" (~s REMOTE-VERSION)
                     "received" (~s version)))

    ((vector _ _ procno type serial status)
     (values serial (packet type procno status (subbytes bstr 24))))))


;; Encodes `packet` structure and writes it to the output
;; stream using correct framing.
(define (write-packet tag value out)
  (let* ((bstr (with-output-bytes
                 (dump message-header (packet-header tag value))
                 (write-bytes (packet-payload value))))
         (length (bytes-length bstr)))
    (dump uint (+ 4 length) out)
    (write-bytes bstr out)
    (flush-output out)))


;; Create header vector for given packet structure,
;; with tag interpreted as the sequence number.
(define (packet-header tag value)
  (match-let (((packet type procno status _) value))
    (vector REMOTE-PROGRAM REMOTE-VERSION procno type tag status)))


;; Use to obtain new sequence number.
(define next-sequence-number
  (let ((lock (make-semaphore 1))
        (next (generator ()
                (for ((i (in-cycle (in-range 1 2147483648))))
                  (yield i)))))
    (λ ()
      (with-semaphore lock
        (next)))))


;; Define procedure that will use use `current-libvirt-connection`
;; to perform a remote procedure call.
(define-syntax-rule (define-caller name procno (arg ...) ret)
  (begin
    (provide
      (contract-out
        (name (-> (type-value/c arg) ... (type-value/c ret)))))
    (define name
      (procedure-rename (make-caller 'name procno ret arg ...) 'name))))

;; Massive variant of the above form.
(define-syntax-rule (define-callers (name procno (arg ...) ret) ...)
  (begin
    (define-caller name procno (arg ...) ret) ...))


;; Create caller procedure with specified types.
(define (make-caller name procno ret-type . arg-types)
  (let ((args-type (apply structure arg-types)))
    (λ args
      (unless (current-libvirt-connection)
        (throw exn:fail name "not connected to libvirt"
                        "(current-libvirt-connection)" (~s #f)))

      (define payload
        (dump/bytes args-type (list->vector args)))

      (define reply
        (sync
          (tandem-call-evt (libvirt-connection-tandem
                             (current-libvirt-connection))
                           (next-sequence-number)
                           (packet 'call procno 'ok payload))))

      (match-let (((packet _ _ status bstr) reply))
        (when (eq? status 'error)
          (raise-libvirt-error name (load/bytes remote-error bstr))))

      (load/bytes ret-type (packet-payload reply)))))


(define (raise-libvirt-error name error)
  (match-let (((vector code domain message level _ ...) error))
    (throw exn:fail name (if (void? message) "error" message))))


; vim:set ts=2 sw=2 et:
