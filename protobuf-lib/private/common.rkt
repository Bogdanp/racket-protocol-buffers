#lang racket/base

(require syntax/readerr
         "lexer.rkt")

(provide
 current-source-name
 raise-parse-error
 expect
 skip)

(define current-source-name
  (make-parameter "<string>"))

(define (raise-parse-error t message)
  (raise-read-error
   message
   (current-source-name)
   (token-line t)
   (token-col t)
   (token-pos t)
   #f))

(define (expected what tok [accessor token-str])
  (define message
    (format "expected ~a but found ~a" what (accessor tok)))
  (raise-parse-error tok message))

(define (expect l type [val #f])
  (define t (lexer-take l))
  (begin0 t
    (unless (eq? (token-type t) type)
      (expected (or val type) t))
    (when (and val (not (equal? (token-val t) val)))
      (expected val t token-val))))

(define (skip . args)
  (void (apply expect args)))
