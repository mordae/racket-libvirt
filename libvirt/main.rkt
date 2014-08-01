#lang racket/base
;
; Libvirt Client
;

(require racket/contract)

(require xdr)

(require "private/protocol.rkt"
         "private/types.rkt")

(provide libvirt-connection?
         current-libvirt-connection
         make-libvirt-connection
         libvirt-connect/unix
         libvirt-connect/tcp)


(define-libvirt-caller connect-open 1 (remote-string uint) nothing)
(define-libvirt-caller connect-close 2 () nothing)
(define-libvirt-caller connect-get-sysinfo 203 (uint) remote-nonnull-string)

(define-libvirt-caller auth-list 66 () (array* remote-auth-type 20))

(define-libvirt-caller node-get-info 6 () remote-node-info)


; vim:set ts=2 sw=2 et:
