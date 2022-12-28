#lang racket/base

(require protobuf/private/ast
         protobuf/private/lexer
         protobuf/private/parser
         racket/runtime-path)

(provide
 parse-example)

(define-runtime-path examples
  "examples")

(define (parse-example name)
  (parameterize ([port-count-lines-enabled #t])
    (call-with-input-file (build-path examples name)
      (lambda (in)
        (->sexp (parse-proto (make-lexer in)))))))
