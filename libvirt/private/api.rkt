#lang racket/base
;
; Libvirt API Definition
;

(require xdr)

(require "protocol.rkt"
         "types.rkt")


(define-callers
  (open 1 (remote-string uint) nothing)
  (close 2 () nothing)
  (node-info 6 () remote-node-info)
  (auth-list 66 () (array* remote-auth-type 20))
  (system-info 203 (uint) remote-nonnull-string))


; vim:set ts=2 sw=2 et:
