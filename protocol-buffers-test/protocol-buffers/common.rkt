#lang racket/base

(require protocol-buffers
         protocol-buffers/private/ast
         protocol-buffers/private/lexer
         protocol-buffers/private/parser
         racket/port
         racket/runtime-path)

(provide
 examples
 parse-example
 roundtrip)

(define-runtime-path examples
  "examples")

(define (parse-example name)
  (define path (build-path examples name))
  (parameterize ([port-count-lines-enabled #t]
                 [current-source-name (path->string path)])
    (call-with-input-file path
      (lambda (in)
        (->sexp (parse-proto (make-lexer in)))))))

(define (roundtrip m v)
  (define bs
    (call-with-output-bytes
     (lambda (out)
       (write-message m v out))))
  (call-with-input-bytes bs
    (lambda (in)
      (read-message m in))))
