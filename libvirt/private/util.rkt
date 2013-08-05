#lang racket/base
;
; Misc Utilities
;

(require racket/contract)

(provide (all-defined-out))



;; String encoding shortcuts.
(define ->b string->bytes/utf-8)
(define ->s bytes->string/utf-8)


; vim:set ts=2 sw=2 et:
