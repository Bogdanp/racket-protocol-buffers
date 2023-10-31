#lang racket/base

(require racket/match
         "lexer.rkt"
         "parser/common.rkt"
         "parser/proto2.rkt"
         "parser/proto3.rkt")

(provide
 current-source-name
 parse-proto)

(define (parse-proto l)
  (match (lexer-peek l)
    [(token _ _ 'syntax _ _ _)
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
        (raise-parse-error syntax-tok "expected 'proto2' or 'proto3'")])]
    [_ (parse-proto2 l)]))
