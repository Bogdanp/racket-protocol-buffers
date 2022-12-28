#lang racket/base

(require "common.rkt"
         "lexer.rkt"
         "parser-v2.rkt"
         "parser-v3.rkt")

(provide
 current-source-name
 parse-proto)

(define (parse-proto l)
  (skip l 'keyword 'syntax)
  (skip l 'equal)
  (define syntax-tok
    (expect l 'string))
  (case (token-val syntax-tok)
    [("proto2")
     (skip l 'semicolon)
     (parse-proto2 l)]
    [("proto3")
     (skip l 'semicolon)
     (parse-proto3 l)]
    [else
     (raise-parse-error syntax-tok (format "expected 'proto2' or 'proto3'~n  found: ~a" (token-str syntax-tok)))]))
