#lang racket/base

(require racket/contract
         racket/port
         "private/lexer.rkt"
         "private/module.rkt"
         "private/parser.rkt")

(provide
 (contract-out
  [read-protobuf-mod (-> input-port? mod?)]

  [mod? (-> any/c boolean?)]
  [mod-package (-> mod? (or/c #f string?))]
  [mod-options (-> mod? hash?)]
  [mod-messages (-> mod? (listof message?))]

  [message? (-> any/c boolean?)]
  [message-name (-> message? symbol?)]
  [message-options (-> message? hash?)]
  [write-message (->* (message? hash?) (output-port?) void?)]))

(define (read-protobuf-mod orig-in)
  (define in (dup-input-port orig-in))
  (port-count-lines! in)
  (make-mod (parse-proto (make-lexer in))))

(define (mod-messages m)
  (filter message? (mod-types m)))

(define (write-message m v [out (current-output-port)])
  ((message-writer m) #f v out))
