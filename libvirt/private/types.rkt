#lang racket/base
;
; RPC Data Types
;

(require racket/contract)

(require xdr)

(provide
  (all-defined-out))


(define message-type
  (enum '((call . 0)
          (reply . 1)
          (event . 2)
          (stream . 3)
          (call-with-fds . 4)
          (reply-with-fds . 5))))

(define message-status
  (enum '((ok . 0)
          (error . 1)
          (continue . 2))))

(define remote-typed-param-type
  (enum '((int . 1)
          (uint . 2)
          (long . 3)
          (ulong . 4)
          (double . 5)
          (boolean . 6)
          (string . 7))))

(define remote-auth-type
  (enum '((none . 0)
          (sasl . 1)
          (polkit . 2))))


(define remote-nonnull-string utf8*)
(define remote-string (optional utf8*))
(define remote-uuid (opaque 16))


(define REMOTE-PROGRAM #x20008086)
(define REMOTE-VERSION 1)


(define message-header
  (structure uint uint uint message-type uint message-status))

(define remote-nonnull-domain
  (structure remote-nonnull-string remote-uuid int))

(define remote-domain
  (optional remote-nonnull-domain))

(define remote-nonnull-network
  (structure remote-nonnull-string remote-uuid))

(define remote-network
  (optional remote-nonnull-network))

(define remote-error
  (structure int int remote-string int remote-domain remote-string
             remote-string remote-string int int remote-network))

(define remote-node-info
  (structure (array uint 32) long int int int int int int))


; vim:set ts=2 sw=2 et:
