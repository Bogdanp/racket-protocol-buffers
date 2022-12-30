#lang racket/base

(require "../ast.rkt"
         "../lexer.rkt"
         "common.rkt")

(provide
 parse-proto2)

(define (parse-proto2 l)
  (let loop ([stmts null])
    (case (token-type (lexer-peek l))
      [(eof)
       (reverse stmts)]
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
    [(extend)
     (parse-extend l)]
    [(service)
     (parse-service l)]
    [else
     (raise-parse-error t "expected a statement")]))

(define (parse-message l)
  (define t (expect l 'keyword 'message))
  (define ident (parse-ident l))
  (define-values (children fields options reserved extensions)
    (parse-message-fields l))
  (Message t ident children fields options reserved extensions))

(define (parse-message-fields l)
  (define t (lexer-peek l))
  (skip l 'lcubrace)
  (let loop ([children null]
             [fields null]
             [options null]
             [reserved null]
             [extensions null])
    (define ft
      (lexer-peek l))
    (case (token-type ft)
      [(rcubrace)
       (skip l 'rcubrace)
       (values
        (reverse children)
        (reverse fields)
        (reverse options)
        (reverse reserved)
        (reverse extensions))]
      [(semicolon)
       (skip l 'semicolon)
       (loop children fields options reserved extensions)]
      [(keyword)
       (case (token-val ft)
         [(option)
          (define option
            (parse-option l))
          (loop children fields (cons option options) reserved extensions)]
         [(reserved)
          (define reserved-node
            (parse-reserved l))
          (loop children fields options (cons reserved-node reserved) extensions)]
         [(extensions)
          (define extensions-node
            (parse-extensions l))
          (loop children fields options reserved (cons extensions-node extensions))]
         [(enum message extend)
          (define child
            (parse-statement l))
          (loop (cons child children) fields options reserved extensions)]
         [else
          (define field
            (parse-message-field l))
          (loop children (cons field fields) options reserved extensions)])]
      [else
       (raise-parse-error t "expected 'option', 'reserved', 'extensions', 'enum', 'message' or a field")])))

(define (parse-message-field l)
  (define t (lexer-peek l))
  (case (token-val t)
    [(required optional repeated)
     (define label
       (token-val (expect l 'keyword)))
     (parse-field* t l label)]
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
     (raise-parse-error t "expected 'required', 'optional', 'repeated', 'oneof' or 'map'")]))

(define (parse-field* t l [label 'optional])
  (case (token-val (lexer-peek l))
    [(group)
     (skip l 'keyword 'group)
     (define name (parse-ident l))
     (skip l 'equals)
     (define number (parse-int l))
     (define-values (children fields options reserved extensions)
       (parse-message-fields l))
     (MessageGroupField t label name number children fields options reserved extensions)]
    [else
     (define type (parse-ident* l))
     (define name (parse-ident l))
     (skip l 'equals)
     (define number (parse-int l))
     (define options (parse-field-options l))
     (skip l 'semicolon)
     (MessageField t label type name number options)]))

(define (parse-oneof-field l)
  (parse-field* (lexer-peek l) l))

(define (parse-extend l)
  (define t (expect l 'keyword 'extend))
  (define name (parse-ident* l))
  (define fields (parse-body l parse-message-field))
  (Extend t name fields))

(define (parse-service l)
  (define t (expect l 'keyword 'service))
  (define name (parse-ident l))
  (skip l 'lcubrace)
  (let loop ([rpcs null]
             [streams null]
             [options null])
    (define ft
      (lexer-peek l))
    (case (token-type ft)
      [(rcubrace)
       (skip l 'rcubrace)
       (Service t name (reverse rpcs) (reverse streams) (reverse options))]
      [(semicolon)
       (skip l 'semicolon)
       (loop rpcs streams options)]
      [(keyword)
       (case (token-val ft)
         [(option)
          (define option
            (parse-option l))
          (loop rpcs streams (cons option options))]
         [(rpc)
          (define rpc
            (parse-rpc l))
          (loop (cons rpc rpcs) streams options)]
         [(stream)
          (define stream
            (parse-stream l))
          (loop rpcs (cons stream streams) options)]
         [else
          (raise-parse-error t "expected 'option', 'rpc' or 'stream'")])]
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

(define (parse-stream l)
  (define t (expect l 'keyword 'stream))
  (define name (parse-ident l))
  (skip l 'lparen)
  (define domain (parse-ident* l))
  (skip l 'comma)
  (define range (parse-ident* l))
  (skip l 'rparen)
  (define options
    (parse-options-or-semicolon l))
  (Stream t name domain range options))
