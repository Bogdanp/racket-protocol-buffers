#lang racket/base

(require "lexer.rkt"
         "parser-common.rkt"
         "proto2.rkt"
         "proto3.rkt")

(provide
 current-source-name
 parse-proto)

(define (parse-proto l)
  (skip l 'keyword 'syntax)
  (skip l 'equals)
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
     (raise-parse-error syntax-tok "expected 'proto2' or 'proto3'")]))
