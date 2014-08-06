#lang racket/base
;
; Libvirt API Definition
;

(require xdr)

(require "protocol.rkt"
         "types.rkt")


(define-callers
  (libvirt-open 1 (remote-string uint) nothing)
  (libvirt-close 2 () nothing)
  (libvirt-node-info 6 () remote-node-info)
  (libvirt-capabilities 7 () remote-nonnull-string)
  (libvirt-auth-list 66 () (array* remote-auth-type 20))
  (libvirt-system-info 203 (uint) remote-nonnull-string))


; vim:set ts=2 sw=2 et:
