#lang racket/base

(require racket/match
         racket/port
         racket/string
         (prefix-in ast: "ast.rkt")
         "lexer.rkt"
         "logger.rkt"
         "parser.rkt"
         "read.rkt"
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
                   [(enum name _options _reader _writer) name]
                   [(message name _options _reader _writer) name]))
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

(struct enum (name options reader writer))

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
  (define reader (make-enum-reader name fields))
  (define writer (make-enum-writer name fields))
  (enum name options reader writer))

(define (make-enum-reader enum-name fields)
  (define values-to-fields
    (for/hasheqv ([(k v) (in-hash fields)])
      (values v k)))
  (lambda (tag in)
    (define v (read-proto-int32 tag in))
    (hash-ref values-to-fields v (λ () (error 'read-enum "invalid value for enum ~a: ~a" enum-name v)))))

(define ((make-enum-writer enum-name fields) num name value out)
  (unless (hash-has-key? fields value)
    (error 'write-enum "enum ~a does not have a case named ~e~n  field: ~a" enum-name value name))
  (write-proto-int32 num name (hash-ref fields value) out))


;; message ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (struct-out message)
 (struct-out message-field))

(struct message-field (name num options reader writer))
(struct message (name options reader writer))

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
  (define-values (fields required repeated)
    (let loop ([fields (hasheq)]
               [required null]
               [repeated null]
               [nodes field-nodes])
      (for/fold ([fields fields]
                 [required required]
                 [repeated repeated])
                ([node (in-list nodes)])
        (match node
          [(ast:MessageField tok label type name number option-nodes)
           (when (reserved? (symbol->string name) number)
             (oops tok "field ~a is reserved" name))
           (define options (make-options option-nodes))
           (define reader (get-reader tok message-env type))
           (define writer (get-writer tok message-env type))
           (define f (message-field name number options reader writer))
           (values (hash-set fields name f)
                   (if (eq? label 'optional) required (cons name required))
                   (if (eq? label 'repeated) (cons name repeated) repeated))]
          [(ast:MessageOneOfField _ _ field-nodes)
           (loop fields field-nodes)]
          [(ast:MessageMapField tok key-type val-type name number option-nodes)
           (when (reserved? (symbol->string name) number)
             (oops tok "field ~a is reserved" name))
           (define options (make-options option-nodes))
           (define k-reader (get-reader tok message-env key-type))
           (define k-writer (get-writer tok message-env key-type))
           (define v-reader (get-reader tok message-env val-type))
           (define v-writer (get-writer tok message-env val-type))
           (define reader (make-map-reader k-reader v-reader))
           (define writer (make-map-writer k-writer v-writer))
           (define f (message-field name number options reader writer))
           (values (hash-set fields name f) required repeated)]
          [_
           (log-protobuf-warning "ignoring field node ~e" node)
           (values fields required repeated)]))))
  (define reader (make-message-reader name fields required repeated))
  (define writer (make-message-writer name fields required repeated))
  (message name options reader writer))

(define (make-message-reader message-name fields required repeated)
  (define numbers-to-fields
    (for/hasheqv ([f (in-hash-values fields)])
      (values (message-field-num f) f)))
  (lambda (in)
    (define-values (missing res)
      (let loop ([missing required]
                 [res (hasheq)])
        (cond
          [(eof-object? (peek-byte in))
           (values missing (for/hasheq ([(k v) (in-hash res)])
                             (if (list? v)
                                 (values k (reverse v))
                                 (values k v))))]
          [else
           (define-values (num tag)
             (read-proto-tag in))
           (match-define (message-field name _number _options reader _writer)
             (hash-ref numbers-to-fields num (λ () (error 'read-message "no field with number ~a in Message ~a" num message-name))))
           (define v
             (reader tag in))
           (loop
            (remq name missing)
            (if (memq name repeated)
                (hash-update res name (λ (vs) (cons v vs)) null)
                (hash-set res name v)))])))
    (begin0 res
      (unless (null? missing)
        (define fields-str (string-join (map symbol->string missing) ", "))
        (error 'read-message "missing required fields for Message ~a: ~a" message-name fields-str)))))

(define ((make-message-writer message-name fields required repeated) name value out)
  (unless (hash? value)
    (if name
        (error 'write-message "expected a hash~n  got: ~e~n  field: ~a" value name)
        (error 'write-message "expected a hash for Message ~a~n  got: ~e" message-name value)))
  (define missing
    (for/fold ([missing required])
              ([(k v) (in-hash value)])
      (match-define (message-field _name num _options _reader writer)
        (hash-ref fields k (λ () (error 'write-message "unknown field ~a for Message ~a" k message-name))))
      (cond
        [(pair? v)
         ;; TODO: Support for packed fields.
         (unless (memv k repeated)
           (error 'write-message "received list but field ~a for Message ~a is not repeated" k message-name))
         (for ([v (in-list v)])
           (writer num name v out))]
        [else
         (writer num name v out)])
      (remq k missing)))
  (unless (null? missing)
    (define fields-str (string-join (map symbol->string missing) ", "))
    (error 'write-message "missing required fields for Message ~a: ~a" message-name fields-str)))

(define ((make-map-reader k-reader v-reader) tag in)
  (define len (read-proto-len 'read-map tag in))
  (define limited-in (make-limited-input-port in len #f))
  (let loop ([res (hash)])
    (cond
      [(eof-object? (peek-byte limited-in)) res]
      [else
       (define-values (k-num k-tag)
         (read-proto-tag limited-in))
       (unless (eqv? k-num 1)
         (error 'read-map "malformed map key"))
       (define k (k-reader k-tag limited-in))
       (define-values (v-num v-tag)
         (read-proto-tag limited-in))
       (unless (eqv? v-num 2)
         (error 'read-map "malformed map value"))
       (define v (v-reader v-tag limited-in))
       (loop (hash-set res k v))])))

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

(define (get-reader tok e t)
  (case t
    [(bool) read-proto-bool]
    [(bytes) read-proto-bytes]
    [(double) read-proto-double]
    [(fixed32) read-proto-fixed32]
    [(fixed64) read-proto-fixed64]
    [(float) read-proto-float]
    [(int32) read-proto-int32]
    [(int64) read-proto-int64]
    [(sfixed32) read-proto-sfixed32]
    [(sfixed64) read-proto-sfixed64]
    [(sint32) read-proto-sint32]
    [(sint64) read-proto-sint64]
    [(string) read-proto-string]
    [(uint32) read-proto-uint32]
    [(uint64) read-proto-uint64]
    [else
     (match (env-ref e t (λ () (oops tok "undefined type ~a" t)))
       [(enum _name _options reader _writer) reader]
       [(message _name _options reader _writer)
        (λ (tag in)
          (define len (read-proto-len 'read-message tag in))
          (reader (make-limited-input-port in len #f)))])]))

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
       [(enum _name _options _reader writer) writer]
       [(message _name _options _reader writer)
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
