#lang racket/base

(require racket/contract
         racket/port
         "private/lexer.rkt"
         "private/module.rkt"
         "private/parser.rkt")

(provide
 (contract-out
  [read-protobuf (-> input-port? mod?)]

  [mod? (-> any/c boolean?)]
  [mod-package (-> mod? (or/c #f string?))]
  [mod-options (-> mod? hash?)]
  [mod-messages (-> mod? (listof message?))]
  [mod-ref (->* (mod? symbol?) ((-> void?)) message?)]

  [message? (-> any/c boolean?)]
  [message-name (-> message? symbol?)]
  [message-options (-> message? hash?)]
  [read-message (->* (message?) (input-port?) hash?)]
  [write-message (->* (message? hash?) (output-port?) void?)]))

(define (read-protobuf orig-in)
  (define in (dup-input-port orig-in))
  (port-count-lines! in)
  (make-mod (parse-proto (make-lexer in))))

(define (mod-messages m)
  (filter message? (mod-types m)))

(define (mod-ref m id [default-proc (λ () (error 'mod-ref "message ~a not found" id))])
  (or
   (for/first ([t (in-list (mod-types m))]
               #:when (message? t)
               #:when (eq? (message-name t) id))
     t)
   (default-proc)))

(define (read-message m [in (current-input-port)])
  ((message-reader m) in))

(define (write-message m v [out (current-output-port)])
  ((message-writer m) #f v out))
