#lang racket/base
;
; Libvirt Client
;

(require racket/contract
         racket/string
         racket/match
         xml)

(require
  (only-in xdr int/c uint/c size/c long/c ulong/c))

(require "private/api.rkt"
         "private/protocol.rkt")

(provide libvirt-connection?
         current-libvirt-connection
         make-libvirt-connection
         libvirt-connect/unix
         libvirt-connect/tcp)

(provide libvirt-close
         libvirt-auth-list)

(provide
  (contract-out
    (rename libvirt-open/wrap libvirt-open
            (->* (string?) (ulong/c) void?))

    (rename libvirt-node-info/wrap libvirt-node-info
            (-> (hash/c symbol? (or/c string? integer?))))

    (rename libvirt-system-info/wrap libvirt-system-info
            (-> xexpr/c))))


(define (libvirt-open/wrap conn (flags 0))
  (libvirt-open conn flags))


;; Turn #"x86_64\0\0" into "x86_64".
(define (c-bytes->string/utf-8 bstr)
  (car (string-split (bytes->string/utf-8 bstr) "\0")))


;; Remove (or trim) whitespace from an xexpr.
(define (strip-whitespace xexpr)
  (match xexpr
    ((cons (regexp #px"^\\s+$") tail)
     (strip-whitespace tail))

    ((cons (and head (regexp #px"(^\\s|\\s$)")) tail)
     (cons (string-trim head)
           (strip-whitespace tail)))

    ((cons head tail)
     (cons (strip-whitespace head)
           (strip-whitespace tail)))

    (else xexpr)))


(define (libvirt-node-info/wrap)
  (match-let (((vector model memory cpus mhz nodes sockets cores threads)
               (libvirt-node-info)))
    (hasheq 'model (c-bytes->string/utf-8
                     (apply bytes model))
            'memory memory
            'cpus cpus
            'mhz mhz
            'nodes nodes
            'sockets sockets
            'cores cores
            'threads threads)))


(define (libvirt-system-info/wrap)
  (strip-whitespace (string->xexpr (libvirt-system-info 0))))


; vim:set ts=2 sw=2 et:
