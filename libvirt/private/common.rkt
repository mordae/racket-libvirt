#lang racket/base
;
; Common Utilities
;

(require racket/contract
         racket/function
         unstable/error)

(provide (all-defined-out))


;; To make exception raising bearable.
(define-syntax-rule (throw constructor name message arg ...)
  (raise-misc-error #:constructor constructor
                    name message arg ...))


;; Serialize procedure invocations using a private semaphore.
(define/contract (synchronized proc)
                 (-> procedure? procedure?)
  (let ((semaphore (make-semaphore 1)))
    (lambda args
      (call-with-semaphore semaphore (thunk (apply proc args))))))


; vim:set ts=2 sw=2 et:
