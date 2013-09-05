#lang racket/base
;
; Misc Utilities
;

(require racket/contract
         racket/string
         xml)

(provide (all-defined-out))


;; String encoding shortcuts.
(define (->bstr str)
  (string->bytes/utf-8 str))

(define (->str bstr)
  (string-trim (bytes->string/utf-8 bstr) #rx"\u0000+"))


;; xexpr encoding shortcut.
(define (->xexpr str)
  (xexpr-strip-whitespace (string->xexpr str)))


;; Strip whitespace from xexpr for the sake of easier processing.
(define (xexpr-strip-whitespace expr)
  (if (pair? expr)
    (if (string? (car expr))
      (if (string=? "" (string-trim (car expr)))
        (xexpr-strip-whitespace (cdr expr))
        (cons (car expr)
              (xexpr-strip-whitespace (cdr expr))))
      (cons (xexpr-strip-whitespace (car expr))
            (xexpr-strip-whitespace (cdr expr))))
    expr))


; vim:set ts=2 sw=2 et:
