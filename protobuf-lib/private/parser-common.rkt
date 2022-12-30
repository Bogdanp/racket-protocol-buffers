#lang racket/base

(require syntax/readerr
         "ast.rkt"
         "lexer.rkt")

;; parsers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 parse-import-qualifier
 parse-enum
 parse-option
 parse-options-or-semicolon
 parse-field-options
 parse-extensions
 parse-reserved
 parse-body
 parse-comma-separated
 parse-range
 parse-ident
 parse-ident*
 parse-int
 parse-signed-int
 parse-signed-number
 parse-constant
 parse-maybe-keyword)

(define (parse-import-qualifier l)
  (define t (lexer-peek l))
  (and (eq? (token-type t) 'keyword)
       (case (token-val t)
         [(public weak)
          (token-val (lexer-take l))]
         [else
          (raise-parse-error t "expected 'weak' or 'public'")])))

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

(define (parse-options-or-semicolon l)
  (case (token-type (lexer-peek l))
    [(lcubrace)
     (parse-body l parse-option)]
    [else
     (begin0 null
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

(define (parse-extensions l)
  (parse-comma-separated 'extensions l parse-range))

(define (parse-reserved l)
  (parse-comma-separated 'reserved l parse-reserved-field))

(define (parse-reserved-field l)
  (define t (lexer-peek l))
  (case (token-type t)
    [(string)
     (token-val (expect l 'string))]
    [(number)
     (parse-range l)]
    [else
     (raise-parse-error t "expected a field name string or a range")]))

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

(define (parse-ident l)
  (token-val (expect l 'ident)))

(define (parse-ident* l)
  (define t (lexer-peek l))
  (case (token-type t)
    [(ident full-ident)
     (token-val (lexer-take l))]
    [else
     (raise-parse-error t "expected an identifier")]))

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

;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
