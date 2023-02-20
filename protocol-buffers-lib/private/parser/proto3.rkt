#lang racket/base

(require "../ast.rkt"
         "../lexer.rkt"
         "../type.rkt"
         "common.rkt")

(provide
 parse-proto3)

(define (parse-proto3 l)
  (define first-tok
    (lexer-peek l))
  (let loop ([stmts null])
    (case (token-type (lexer-peek l))
      [(eof)
       (Proto3 first-tok (reverse stmts))]
      [(semicolon)
       (skip l 'semicolon)
       (loop stmts)]
      [else
       (loop (cons (parse-statement l) stmts))])))

(define (parse-statement l)
  (define t
    (lexer-peek l))
  (case (token-val t)
    [(import)
     (skip l 'keyword 'import)
     (define qualifier
       (parse-import-qualifier l))
     (begin0 (Import t qualifier (token-val (expect l 'string)))
       (skip l 'semicolon))]
    [(package)
     (skip l 'keyword 'package)
     (begin0 (Package t (parse-ident* l))
       (skip l 'semicolon))]
    [(option)
     (parse-option l)]
    [(message)
     (parse-message l)]
    [(enum)
     (parse-enum l)]
    [(service)
     (parse-service l)]
    [else
     (raise-parse-error t "expected a statement")]))

(define (parse-message l)
  (define t (expect l 'keyword 'message))
  (define ident (parse-ident l))
  (define-values (children fields options reserved)
    (parse-message-fields l))
  (Message t ident children fields options reserved null))

(define (parse-message-fields l)
  (skip l 'lcubrace)
  (let loop ([children null]
             [fields null]
             [options null]
             [reserved null])
    (define ft
      (lexer-peek l))
    (case (token-type ft)
      [(rcubrace)
       (skip l 'rcubrace)
       (values
        (reverse children)
        (reverse fields)
        (reverse options)
        (reverse reserved))]
      [(semicolon)
       (skip l 'semicolon)
       (loop children fields options reserved)]
      [(keyword)
       (case (token-val ft)
         [(option)
          (define option
            (parse-option l))
          (loop children fields (cons option options) reserved)]
         [(reserved)
          (define reserved-node
            (parse-reserved l))
          (loop children fields options (cons reserved-node reserved))]
         [(enum message)
          (define child
            (parse-statement l))
          (loop (cons child children) fields options reserved)]
         [else
          (define field
            (parse-message-field l))
          (loop children (cons field fields) options reserved)])]
      [else
       (define field
         (parse-message-field l))
       (loop children (cons field fields) options reserved)])))

(define (parse-message-field l)
  (define t (lexer-peek l))
  (case (token-val t)
    [(oneof)
     (skip l 'keyword 'oneof)
     (define name (parse-ident l))
     (define fields (parse-body l parse-oneof-field))
     (MessageOneOfField t name fields)]
    [(map)
     (skip l 'keyword 'map)
     (skip l 'langbrace)
     (define key-type (parse-ident* l))
     (skip l 'comma)
     (define val-type (parse-ident l))
     (skip l 'rangbrace)
     (define name (parse-ident l))
     (skip l 'equals)
     (define number (parse-int l))
     (define options (parse-field-options l))
     (skip l 'semicolon)
     (MessageMapField t key-type val-type name number options)]
    [else
     (define label
       (or (parse-maybe-keyword l 'repeated) 'optional))
     (define type (parse-ident* l))
     (define name (parse-ident l))
     (skip l 'equals)
     (define number (parse-int l))
     (define options
       (let ([options (parse-field-options l)])
         (if (and (eq? label 'repeated) (scalar-type? type))
             (cons (Option #f '(packed) #t) options)
             options)))
     (skip l 'semicolon)
     (MessageField t label type name number options)]))

(define (parse-oneof-field l)
  (define t (lexer-peek l))
  (define type (parse-ident* l))
  (define name (parse-ident l))
  (skip l 'equals)
  (define number (parse-int l))
  (define options (parse-field-options l))
  (skip l 'semicolon)
  (MessageField t 'optional type name number options))

(define (parse-service l)
  (define t (expect l 'keyword 'service))
  (define name (parse-ident l))
  (skip l 'lcubrace)
  (let loop ([rpcs null]
             [options null])
    (define ft
      (lexer-peek l))
    (case (token-type ft)
      [(rcubrace)
       (skip l 'rcubrace)
       (Service t name (reverse rpcs) null (reverse options))]
      [(semicolon)
       (skip l 'semicolon)
       (loop rpcs options)]
      [(keyword)
       (case (token-val ft)
         [(option)
          (define option
            (parse-option l))
          (loop rpcs (cons option options))]
         [(rpc)
          (define rpc
            (parse-rpc l))
          (loop (cons rpc rpcs) options)]
         [else
          (raise-parse-error t "expected 'option' or 'rpc'")])]
      [else
       (raise-parse-error t "expected service definition")])))

(define (parse-rpc l)
  (define t (expect l 'keyword 'rpc))
  (define name (parse-ident l))
  (skip l 'lparen)
  (define stream-domain?
    (and (parse-maybe-keyword l 'stream) #t))
  (define domain (parse-ident* l))
  (skip l 'rparen)
  (skip l 'keyword 'returns)
  (skip l 'lparen)
  (define stream-range?
    (and (parse-maybe-keyword l 'stream) #t))
  (define range (parse-ident* l))
  (skip l 'rparen)
  (define options
    (parse-options-or-semicolon l))
  (RPC t name stream-domain? domain stream-range? range options))
