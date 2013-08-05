#lang racket/base
;
; Misc Utilities
;

(require racket/contract
         racket/string)

(provide (all-defined-out))


;; String encoding shortcuts.
(define (->b str)
  (string->bytes/utf-8 str))

(define (->s bstr)
  (string-trim (bytes->string/utf-8 bstr) #rx"\u0000+"))


; vim:set ts=2 sw=2 et:
