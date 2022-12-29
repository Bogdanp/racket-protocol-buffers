#lang racket/base

(require "ast.rkt"
         "common.rkt"
         "lexer.rkt")

(provide
 parse-proto3)

(define (parse-proto3 l)
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
       (let ([qt (lexer-peek l)])
         (and (eq? (token-type qt) 'keyword)
              (case (token-val qt)
                [(weak public)
                 (token-val (expect l 'keyword))]
                [else
                 (raise-parse-error qt "expected 'weak', 'public' or import string")]))))
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

(define (parse-option l)
  (define t (expect l 'keyword 'option))
  (begin0 (parse-option* t l)
    (skip l 'semicolon)))

(define (parse-option* t l)
  (define path (parse-option-path l))
  (skip l 'equals)
  (define value (parse-constant l))
  (Option t path value))

(define (parse-option-path l)
  (let loop ([path (list (parse-option-start l))])
    (case (token-type (lexer-peek l))
      [(dot)
       (skip l 'dot)
       (loop (cons (parse-ident l) path))]
      [else
       (reverse path)])))

(define (parse-option-start l)
  (define t
    (lexer-peek l))
  (case (token-type t)
    [(ident)
     (parse-ident l)]
    [(lparen)
     (skip l 'lparen)
     (begin0 (parse-ident* l)
       (skip l 'rparen))]
    [else
     (raise-parse-error t "expected an open paren or an identifier")]))

(define (parse-enum l)
  (define t (expect l 'keyword 'enum))
  (define ident
    (parse-ident l))
  (skip l 'lcubrace)
  (let loop ([fields null]
             [options null]
             [reserved null])
    (define ft
      (lexer-peek l))
    (case (token-type ft)
      [(rcubrace)
       (skip l 'rcubrace)
       (Enum t ident (reverse fields) (reverse options) (reverse reserved))]
      [(semicolon)
       (skip l 'semicolon)
       (loop fields options reserved)]
      [(keyword)
       (case (token-val ft)
         [(option)
          (define option
            (parse-option l))
          (loop fields (cons option options) reserved)]
         [(reserved)
          (define reserved-node
            (parse-reserved l))
          (loop fields options (cons reserved-node reserved))]
         [else
          (raise-parse-error t "expected 'option' or 'reserved'")])]
      [(ident)
       (define field (parse-enum-field l))
       (skip l 'semicolon)
       (loop (cons field fields) options reserved)]
      [else
       (raise-parse-error t "expected 'option', 'reserved' or a field")])))

(define (parse-enum-field l)
  (define t (lexer-peek l))
  (define ident (parse-ident l))
  (skip l 'equals)
  (define value (parse-signed-int l))
  (EnumField t ident value (parse-field-options l)))

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
     (define options (parse-field-options l))
     (skip l 'semicolon)
     (MessageField t label type name number options)]))

(define (parse-body l entry-proc)
  (skip l 'lcubrace)
  (let loop ([entries null])
    (define entry
      (entry-proc l))
    (define ft
      (lexer-peek l))
    (case (token-type ft)
      [(rcubrace)
       (skip l 'rcubrace)
       (reverse (cons entry entries))]
      [(semicolon)
       (skip l 'semicolon)
       (loop entries)]
      [else
       (loop (cons entry entries))])))

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

(define (parse-options-or-semicolon l)
  (case (token-type (lexer-peek l))
    [(lcubrace) (parse-body l parse-option)]
    [else (begin0 null
            (skip l 'semicolon))]))

(define (parse-field-options l)
  (case (token-type (lexer-peek l))
    [(lsqbrace) (parse-field-options* l)]
    [else null]))

(define (parse-field-options* l)
  (skip l 'lsqbrace)
  (let loop ([options null])
    (define option
      (parse-option* (lexer-peek l) l))
    (case (token-type (lexer-peek l))
      [(rsqbrace)
       (skip l 'rsqbrace)
       (reverse (cons option options))]
      [(comma)
       (skip l 'comma)
       (loop (cons option options))]
      [else
       (raise-parse-error (lexer-peek l) "expected comma or closing square bracket")])))

(define (parse-reserved l)
  (parse-comma-separated 'reserved l parse-reserved-field))

(define (parse-comma-separated who l field-proc)
  (skip l 'keyword who)
  (let loop ([fields null])
    (define field
      (field-proc l))
    (define nt
      (lexer-peek l))
    (case (token-type nt)
      [(semicolon)
       (skip l 'semicolon)
       (reverse (cons field fields))]
      [(comma)
       (skip l 'comma)
       (loop (cons field fields))]
      [else
       (raise-parse-error nt "expected a comma or a semicolon")])))

(define (parse-reserved-field l)
  (define t (lexer-peek l))
  (case (token-type t)
    [(string)
     (token-val (expect l 'string))]
    [(number)
     (parse-range l)]
    [else
     (raise-parse-error t "expected a field name string or a range")]))

(define (parse-range l)
  (define t (lexer-peek l))
  (define lo (parse-int l))
  (case (token-val (lexer-peek l))
    [(to)
     (skip l 'keyword 'to)
     (case (token-val (lexer-peek l))
       [(max)
        (skip l 'keyword 'max)
        (Range t lo 'max)]
       [else
        (Range t lo (parse-int l))])]
    [else
     lo]))

(define (parse-ident* l)
  (define t (lexer-peek l))
  (case (token-type t)
    [(ident full-ident)
     (token-val (lexer-take l))]
    [else
     (raise-parse-error t "expected an identifier")]))

(define (parse-ident l)
  (token-val (expect l 'ident)))

(define (parse-sign l)
  (case (token-type (lexer-peek l))
    [(plus)
     (begin0 1
       (skip l 'plus))]
    [(minus)
     (begin0 -1
       (skip l 'minus))]
    [else 1]))

(define (parse-int l)
  (define t (expect l 'number))
  (begin0 (token-val t)
    (unless (exact-integer? (token-val t))
      (raise-parse-error t "expected an integer"))))

(define (parse-signed-int l)
  (* (parse-sign l) (parse-int l)))

(define (parse-signed-number l)
  (* (parse-sign l) (token-val (expect l 'number))))

(define (parse-constant l)
  (define t
    (lexer-peek l))
  (case (token-type t)
    [(ident full-ident)
     (parse-ident* l)]
    [(minus plus)
     (parse-signed-number l)]
    [(boolean string number)
     (token-val (lexer-take l))]
    [else
     (raise-parse-error t "expected a full identifier or a literal")]))

(define (parse-maybe-keyword l name)
  (define t (lexer-peek l))
  (and (eq? (token-type t) 'keyword)
       (token-val (expect l 'keyword name))))
