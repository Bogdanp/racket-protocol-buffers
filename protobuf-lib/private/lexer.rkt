#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/match
         racket/port
         racket/string)

(provide
 (struct-out exn:fail:lexer)
 (struct-out token)

 make-lexer
 lexer-peek
 lexer-take)

(struct exn:fail:lexer exn:fail (line col pos)
  #:transparent)

(define (raise-lexer-error message line col pos)
  (raise (exn:fail:lexer message (current-continuation-marks) line col pos)))

(struct token (type str val line col pos)
  #:prefab)

(struct lexer
  (in
   skip-comments?
   partial-strings?
   [pending #:mutable])
  #:transparent)

(define (make-lexer in
                    #:skip-comments? [skip-comments? #t]
                    #:partial-strings? [partial-strings? #f])
  (lexer in skip-comments? partial-strings? #f))

(define (lexer-peek l)
  (cond
    [(lexer-pending l) => values]
    [else
     (define pending (lexer-read-token l))
     (begin0 pending
       (set-lexer-pending! l pending))]))

(define (lexer-take l)
  (cond
    [(lexer-pending l)
     => (lambda (pending)
          (begin0 pending
            (set-lexer-pending! l #f)))]

    [else
     (lexer-read-token l)]))

(define (lexer-read-token l)
  (define skip-comments?
    (lexer-skip-comments? l))
  (define partial-strings?
    (lexer-partial-strings? l))
  (let loop ()
    (define t
      (read-token (lexer-in l) partial-strings?))
    (case (token-type t)
      [(comment whitespace)
       (if skip-comments? (loop) t)]
      [else
       t])))


;; readers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-token in [partial-strings? #f])
  (define-values (line col pos)
    (port-next-location in))

  (define (make-token type str [val str])
    (token type str val line col pos))

  (match (peek-char in)
    [(? eof-object?) (make-token 'eof        (read-string 1 in))]
    [(? whitespace?) (make-token 'whitespace (read-whitespace in))]

    [#\/ #:when (equal? (peek-string 2 0 in) "//")
     (make-token 'comment (read-line in))]

    [#\+ (make-token 'plus       (read-string 1 in))]
    [#\- (make-token 'minus      (read-string 1 in))]
    [#\= (make-token 'equals     (read-string 1 in))]
    [#\; (make-token 'semicolon  (read-string 1 in))]
    [#\( (make-token 'lparen     (read-string 1 in))]
    [#\) (make-token 'rparen     (read-string 1 in))]
    [#\[ (make-token 'lsqbrace   (read-string 1 in))]
    [#\] (make-token 'rsqbrace   (read-string 1 in))]
    [#\{ (make-token 'lcubrace   (read-string 1 in))]
    [#\} (make-token 'rcubrace   (read-string 1 in))]
    [#\< (make-token 'langbrace  (read-string 1 in))]
    [#\> (make-token 'rangbrace  (read-string 1 in))]
    [#\, (make-token 'comma      (read-string 1 in))]

    [#\. #:when (not (decimal-digit? (peek-char in 1)))
     (make-token 'dot (read-string 1 in))]

    [(or #\' #\")
     (define-values (s v)
       (proto:read-string in partial-strings?))
     (make-token 'string s v)]

    [(? number-start?)
     (define-values (s v)
       (proto:read-number in))
     (make-token 'number s v)]

    [(? ident-start?)
     (define-values (s v)
       (proto:read-ident in))

     (case v
       [(nan)
        (make-token 'number s +nan.f)]
       [(inf)
        (make-token 'number s +inf.0)]
       [(true false)
        (make-token 'boolean s (eq? v 'true))]
       [(syntax import weak public package option message enum extend required optional
                repeated group oneof map extensions to max reserved service rpc returns)
        (make-token 'keyword s v)]
       [else
        (if (string-contains? s ".")
            (make-token 'full-ident s v)
            (make-token 'ident s v))])]

    [c
     (raise-lexer-error (format "unexpected character: ~a" c) line col pos)]))


;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-whitespace in)
  (take-while in whitespace?))

(define (take-while in p)
  (define-values (line col pos)
    (port-next-location in))

  (with-output-to-string
    (lambda ()
      (let loop ([p p]
                 [c (peek-char in)]
                 [span 0])
        (define next-p
          (with-handlers ([exn:fail? (λ (e) (raise-lexer-error (exn-message e) line col pos))])
            (p c)))

        (when next-p
          (display (read-char in))
          (loop next-p (peek-char in) (add1 span)))))))


;; matchers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (λcase stx)
  (syntax-parse stx
    #:literals (else)
    [(_ {~optional {~seq #:char-id char-id}}
        [(lit ...) e ...] ...
        {~optional [else else-e ...]})
     #:with c #'{~? char-id next-c}
     #'(λ (c)
         (case c
           [(lit ...) e ...] ...
           {~? [else else-e ...]
               [else #f]}))]))

(define-syntax (define-λcase stx)
  (syntax-parse stx
    [(_ name:id . rest)
     #'(define name (λcase . rest))]))

(define-λcase whitespace?
  [(#\u00A0 #\space #\tab #\newline #\return) whitespace?])

(define ((make-ident-predicate [char-categories '(ll lu nd)]) c)
  (and
   (char? c)
   (or (char=? c #\_)
       (char=? c #\.)
       (member (char-general-category c) char-categories))
   ident-more?))

(define ident-start? (make-ident-predicate '(ll lu)))
(define ident-more?  (make-ident-predicate '(ll lu nd)))

(define-λcase number-start?
  [(#\.) (number-more? #\.)]
  [(#\0) (λcase
          [(#\x #\X) (λ (c) (or (hex-digit? c) (error "expected a hex digit")))]
          [(#\.) decimal-digit-or-exponent?]) ]
  [(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) number-more?])

(define-λcase number-more?
  #:char-id c
  [(#\e #\E) decimal-digit-or-sign?]
  [(#\.) (λ (next-c)
           (or (decimal-digit-or-exponent? next-c)
               (error "expected a digit")))]
  [else (and (decimal-digit? c) number-more?)])

(define-λcase decimal-digit-or-sign?
  #:char-id c
  [(#\+ #\-) decimal-digit?]
  [else (and (decimal-digit? c) decimal-digit?)])

(define-λcase decimal-digit-or-exponent?
  #:char-id c
  [(#\e #\E) decimal-digit-or-sign?]
  [else (and (decimal-digit? c) decimal-digit-or-exponent?)])

(define-λcase decimal-digit?
  [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) decimal-digit?])

(define-λcase hex-digit?
  #:char-id c
  [(#\a #\b #\c #\d #\e #\f) hex-digit?]
  [(#\A #\B #\C #\D #\E #\F) hex-digit?]
  [else (and (decimal-digit? c) hex-digit?)])

(define-λcase octal-digit?
  [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7) octal-digit?])


;; readers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((make-reader p f) in)
  (define s (take-while in p))
  (values s (f s)))

(define proto:read-ident
  (make-reader ident-start? string->symbol))

(define proto:read-number
  (make-reader number-start? (λ (s)
                               (if (regexp-match? #rx"^0[xX]" s)
                                   (string->number (string-append "#x" (substring s 2)) 16)
                                   (string->number s)))))

(define (proto:read-string in [partial? #f])
  (define quote-char (read-char in))
  (define lit-str (open-output-string))
  (define actual-str (open-output-string))
  (write-char quote-char lit-str)
  (write-char quote-char actual-str)
  (define has-end-quote?
    (let loop ([escaped? #f])
      (define char
        (read-char in))
      (cond
        [(eof-object? char)
         (cond
           [partial? #f]
           [else (error 'proto:read-string "unexpected EOF while reading string")])]
        [escaped?
         (define-values (escape-seq escape-char)
           (proto:string-escape char in))
         (write-string escape-seq lit-str)
         (write-char escape-char actual-str)
         (loop #f)]
        [(eqv? char #\\)
         (loop #t)]
        [else
         (write-char char lit-str)
         (write-char char actual-str)
         (cond
           [(eqv? char quote-char) #t]
           [else  (loop #f)])])))
  (define str
    (let ([str (get-output-string actual-str)])
      (substring str 1 ((if has-end-quote? sub1 values) (string-length str)))))
  (values (get-output-string lit-str) str))

(define (proto:string-escape chr in)
  (case chr
    [(#\\) (values "\\\\" #\\)]
    [(#\a) (values "\\a"  #\u007)]
    [(#\b) (values "\\b"  #\backspace)]
    [(#\f) (values "\\f"  #\page)]
    [(#\r) (values "\\r"  #\return)]
    [(#\n) (values "\\n"  #\newline)]
    [(#\t) (values "\\t"  #\tab)]
    [(#\v) (values "\\v"  #\vtab)]
    [(#\x #\X)
     (define-values (seq code)
       (read-hex-char-code (make-limited-input-port in 2 #f)))
     (define lit-str
       (string-append (string #\\ chr) seq))
     (unless code
       (error 'proto:string-escape "unexpected EOF while reading string hex escape"))
     (values lit-str (integer->char code))]
    [else
     (cond
       [(octal-digit? chr)
        (define buf (open-output-bytes))
        (write-char chr buf)
        (let ([digits (read-string 2 in)])
          (when (eof-object? digits)
            (error 'proto:string-escape "unexpected EOF while reading string octal escape"))
          (write-string digits buf))
        (define-values (seq code)
          (read-oct-char-code (open-input-bytes (get-output-bytes buf))))
        (define lit-str
          (string-append (string #\\) seq))
        (unless code
          (error 'proto:string-escape "unexpected EOF while reading string octal escape"))
        (values lit-str (integer->char code))]
       [else
        (values (string #\\ chr) chr)])]))

(define read-hex-char-code
  (make-reader hex-digit? (λ (digits) (string->number digits 16))))

(define read-oct-char-code
  (make-reader octal-digit? (λ (digits) (string->number digits 8))))
