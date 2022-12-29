#lang racket/base

(require protobuf/private/ast
         protobuf/private/lexer
         protobuf/private/parser
         racket/runtime-path)

(provide
 examples
 parse-example)

(define-runtime-path examples
  "examples")

(define (parse-example name)
  (define path (build-path examples name))
  (parameterize ([port-count-lines-enabled #t]
                 [current-source-name (path->string path)])
    (call-with-input-file path
      (lambda (in)
        (->sexp (parse-proto (make-lexer in)))))))
