#lang racket/base

(require racket/match
         (prefix-in ast: "ast.rkt")
         "lexer.rkt"
         "logger.rkt"
         "parser.rkt")

(provide
 make-mod
 mod?
 mod-package
 mod-options
 mod-types)

(struct mod (package options types))

(define (make-mod nodes)
  (define env (make-env))
  (define-values (package options types)
    (for/fold ([package #f]
               [options (hash)]
               [types null])
              ([node (in-list nodes)])
      (match node
        [(ast:Import _ qualifier path)
         (call-with-input-file path
           (lambda (in)
             (define l (make-lexer in))
             (define m (make-mod (parse-proto l)))
             (for ([e (in-list (mod-types m))])
               (define name
                 (match e
                   [(enum name _options _fields) name]
                   [(message name _options _fields) name]))
               (env-set! env name e))
             (if (eq? qualifier 'public)
                 (values package options (append types (reverse (mod-types m))))
                 (values package options types))))]
        [(ast:Package tok _)
         #:when package
         (oops tok "package already declared")]
        [(ast:Package _ name)
         (values name options types)]
        [(ast:Option _ path value)
         (values package (hash-set options path value) types)]
        [(ast:Enum _ name fields options reserved)
         (define e (make-enum name fields options reserved))
         (env-set! env name e)
         (values package options (cons e types))]
        [(ast:Message _ name children fields options reserved extensions)
         (define m (make-message name children fields options reserved extensions env))
         (env-set! env name m)
         (values package options (cons m types))]
        [(ast:RPC _ name _stream-domain? _domain _stream-range _range? _options)
         (log-protobuf-warning "skipping RPC definition ~a" name)
         (values package options types)]
        [(ast:Stream _ name _domain _range _options)
         (log-protobuf-warning "skipping Stream definition ~a" name)
         (values package options types)]
        [(ast:Service _ name _rpcs _streams _options)
         (log-protobuf-warning "skipping Service definition ~a" name)
         (values package options types)]
        [_
         (error 'make-mod "unexpected node: ~e" node)])))
  (mod package options types))


;; enum ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (struct-out enum))

(struct enum (name options fields))

(define (make-enum name field-nodes option-nodes reserved)
  (define reserved? (make-reserved?-proc reserved))
  (define options (make-options option-nodes))
  (define fields
    (for/hasheq ([node (in-list field-nodes)])
      (match node
        [(ast:EnumField tok name value _options)
         (when (reserved? (symbol->string name) value)
           (oops tok "field ~a is reserved" name))
         (values name value)])))
  (enum name options fields))


;; message ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (struct-out map-type)
 (struct-out message)
 (struct-out message-field))

(struct map-type (k-type v-type))
(struct message-field (label type name options))
(struct message (name options fields))

(define (make-message name children field-nodes option-nodes reserved _extensions env)
  (define reserved? (make-reserved?-proc reserved))
  (define options (make-options option-nodes))
  (define message-env (make-env env))
  (for ([node (in-list children)])
    (match node
      [(ast:Enum _ name fields options reserved)
       (define e (make-enum name fields options reserved))
       (env-set! message-env name e)]
      [(ast:Message _ name children fields options reserved extensions)
       (define m (make-message name children fields options reserved extensions message-env))
       (env-set! message-env name m)]
      [_
       (log-protobuf-warning "ignoring node ~e (child of message ~a)" node name)]))
  (define fields
    (let loop ([fields (hasheqv)]
               [nodes field-nodes])
      (for/fold ([fields fields])
                ([node (in-list nodes)])
        (match node
          [(ast:MessageField tok label type name number options)
           (when (reserved? (symbol->string name) number)
             (oops tok "field ~a is reserved" name))
           (define f (message-field label (get-type tok message-env type) name (make-options options)))
           (hash-set fields number f)]
          [(ast:MessageOneOfField _ _ field-nodes)
           ;; FIXME: enforce disjunction?
           (loop fields field-nodes)]
          [(ast:MessageMapField tok key-type val-type name number options)
           (when (reserved? (symbol->string name) number)
             (oops tok "field ~a is reserved" name))
           (define k (get-type tok message-env key-type))
           (define v (get-type tok message-env val-type))
           (define f (message-field #f (map-type k v) name (make-options options)))
           (hash-set fields number f)]
          [_
           (log-protobuf-warning "ignoring field node ~e" node)]))))
  (message name options fields))

(define (get-type tok e t)
  (case t
    [(double float int32 int64 uint32 uint64 sint32 sint64 fixed32 fixed64 sfixed32 sfixed64 bool string bytes) t]
    [else (env-ref e t (λ () (oops tok "unknown type ~a" t)))]))


;; env ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct env (parent bindings))

(define (make-env [parent #f])
  (env parent (make-hasheq)))

(define (env-ref e id [default-proc (λ () (error 'env-ref "binding not found: ~a" id))])
  (match-define (env parent bindings) e)
  (cond
    [(hash-has-key? bindings id)
     (hash-ref bindings id)]
    [(not parent)
     (default-proc)]
    [else
     (env-ref parent id)]))

(define (env-set! e id v)
  (hash-set! (env-bindings e) id v))


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-options option-nodes)
  (for/hash ([node (in-list option-nodes)])
    (match node
      [(ast:Option _ path value)
       (values path value)])))

(define (make-reserved?-proc reserved)
  (let loop ([reserved (apply append reserved)])
    (match reserved
      ['()
       (lambda (_name _number) #f)]
      [(cons (? integer? reserved-number) reserved-rest)
       (define rest-proc
         (loop reserved-rest))
       (lambda (name number)
         (or (and (= number reserved-number) number)
             (rest-proc name number)))]
      [(cons (? string? reserved-name) reserved-rest)
       (define rest-proc
         (loop reserved-rest))
       (lambda (name number)
         (or (and (string=? name reserved-name) reserved-name)
             (rest-proc name number)))]
      [(cons (ast:Range _ lo hi) reserved-rest)
       (define rest-proc
         (loop reserved-rest))
       (lambda (name number)
         (or (and
              (and (>= number lo)
                   (<= number hi))
              (list lo hi))
             (rest-proc name number)))])))

(define (oops tok fmt . args)
  (match-define (token _ _ _ line col pos) tok)
  (define src (current-source-name))
  (define msg (apply format fmt args))
  (if (and line col)
      (format "~a~n  source: ~a~n  line: ~a~n  column: ~a" msg src line col)
      (format "~a~n  source: ~a~n  position: ~a" msg src pos)))
