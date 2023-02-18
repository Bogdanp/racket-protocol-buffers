#lang racket/base

(require protocol-buffers/private/ast
         protocol-buffers/private/lexer
         protocol-buffers/private/parser
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
