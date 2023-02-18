#lang racket/base

(require racket/match
         racket/port
         racket/string
         (prefix-in ast: "ast.rkt")
         "lexer.rkt"
         "logger.rkt"
         "parser.rkt"
         "write.rkt")

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

(struct enum (name options writer))

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
  (enum name options (make-enum-writer name fields)))

(define ((make-enum-writer enum-name fields) num name value out)
  (unless (hash-has-key? fields value)
    (error 'write-enum "enum ~a does not have a case named ~e~n  field: ~a" enum-name value name))
  (write-proto-int32 num name (hash-ref fields value) out))


;; message ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (struct-out message)
 (struct-out message-field))

(struct message-field (label num options writer))
(struct message (name options writer))

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
    (let loop ([fields (hasheq)]
               [nodes field-nodes])
      (for/fold ([fields fields])
                ([node (in-list nodes)])
        (match node
          [(ast:MessageField tok label type name number option-nodes)
           (when (reserved? (symbol->string name) number)
             (oops tok "field ~a is reserved" name))
           (define options (make-options option-nodes))
           (define writer (get-writer tok message-env type))
           (define f (message-field label number options writer))
           (hash-set fields name f)]
          [(ast:MessageOneOfField _ _ field-nodes)
           ;; FIXME: enforce disjunction?
           (loop fields field-nodes)]
          [(ast:MessageMapField tok key-type val-type name number option-nodes)
           (when (reserved? (symbol->string name) number)
             (oops tok "field ~a is reserved" name))
           (define options (make-options option-nodes))
           (define k-writer (get-writer tok message-env key-type))
           (define v-writer (get-writer tok message-env val-type))
           (define writer (make-map-writer k-writer v-writer))
           (define f (message-field #f number options writer))
           (hash-set fields name f)]
          [_
           (log-protobuf-warning "ignoring field node ~e" node)]))))
  (message name options (make-message-writer name fields)))

(define (make-message-writer message-name fields)
  (define-values (required-fields repeated-fields)
    (for/fold ([required-fields null]
               [repeated-fields null])
              ([(name f) (in-hash fields)])
      (match-define (message-field label _num _options _writer) f)
      (values
       (if (eq? label 'optional) required-fields (cons name required-fields))
       (if (eq? label 'repeated) (cons name repeated-fields) repeated-fields))))
  (lambda (name value out)
    (unless (hash? value)
      (if name
          (error 'write-message "expected a hash~n  got: ~e~n  field: ~a" value name)
          (error 'write-message "expected a hash for Message ~a~n  got: ~e" message-name value)))
    (define missing-required-fields
      (for/fold ([missing-required-fields required-fields])
                ([(k v) (in-hash value)])
        (match-define (message-field _label num _options writer)
          (hash-ref fields k (λ () (error 'write-message "unknown field ~a for Message ~a" k message-name))))
        (cond
          [(pair? v)
           ;; TODO: Support for packed fields.
           (unless (memv k repeated-fields)
             (error 'write-message "received list but field ~a for Message ~a is not repeated" k message-name))
           (for ([v (in-list v)])
             (writer num name v out))]
          [else
           (writer num name v out)])
        (remq k missing-required-fields)))
    (unless (null? missing-required-fields)
      (define fields-str (string-join (map symbol->string missing-required-fields) ", "))
      (error 'write-message "missing required fields for Message ~a: ~a" message-name fields-str))))

(define ((make-map-writer k-writer v-writer) num name value out)
  (unless (hash? value)
    (error 'write-map "expected a hash~n  got: ~e~n  field: ~a" value name))
  (define bs
    (call-with-output-bytes
     (lambda (bs-out)
       (for ([(k v) (in-hash value)])
         (k-writer 1 "map key" k bs-out)
         (v-writer 2 "map value" v bs-out)))))
  (write-proto-bytes num name bs out))

(define (get-writer tok e t)
  (case t
    [(bool) write-proto-bool]
    [(bytes) write-proto-bytes]
    [(double) write-proto-double]
    [(fixed32) write-proto-fixed32]
    [(fixed64) write-proto-fixed64]
    [(float) write-proto-float]
    [(int32) write-proto-int32]
    [(int64) write-proto-int64]
    [(sfixed32) write-proto-sfixed32]
    [(sfixed64) write-proto-sfixed64]
    [(sint32) write-proto-sint32]
    [(sint64) write-proto-sint64]
    [(string) write-proto-string]
    [(uint32) write-proto-uint32]
    [(uint64) write-proto-uint64]
    [else
     (match (env-ref e t (λ () (oops tok "undefined type ~a" t)))
       [(enum _name _options writer) writer]
       [(message _name _options writer)
        (λ (num name value out)
          (define bs
            (call-with-output-bytes
             (lambda (bs-out)
               (writer name value bs-out))))
          (write-proto-bytes num name bs out))])]))


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
