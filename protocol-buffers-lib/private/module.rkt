#lang racket/base

(require racket/fixnum
         racket/match
         racket/port
         racket/string
         "ast.rkt"
         "common.rkt"
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
        [(Import _ qualifier path)
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
        [(Package tok _)
         #:when package
         (oops tok "package already declared")]
        [(Package _ name)
         (values name options types)]
        [(Option _ path value)
         (values package (hash-set options path value) types)]
        [(Enum _ name fields options reserved)
         (define e (make-enum name fields options reserved))
         (env-set! env name e)
         (values package options (cons e types))]
        [(Message _ name children fields options reserved extensions)
         (define m (make-message name children fields options reserved extensions env))
         (env-set! env name m)
         (values package options (cons m types))]
        [(RPC _ name _stream-domain? _domain _stream-range _range? _options)
         (log-protobuf-warning "skipping RPC definition ~a" name)
         (values package options types)]
        [(Stream _ name _domain _range _options)
         (log-protobuf-warning "skipping Stream definition ~a" name)
         (values package options types)]
        [(Service _ name _rpcs _streams _options)
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
        [(EnumField tok name value _options)
         (when (reserved? (symbol->string name) value)
           (oops tok "field ~a is reserved" name))
         (values name value)])))
  (define reader (make-enum-reader name fields))
  (define writer (make-enum-writer name fields))
  (enum name options reader writer))

(define (make-enum-reader ename fields)
  (define values-to-fields
    (for/hasheqv ([(k v) (in-hash fields)])
      (values v k)))
  (lambda (tag in)
    (define v (read-proto-int32 tag in))
    (hash-ref values-to-fields v (λ () (error 'read-enum "invalid value for enum ~a: ~a" ename v)))))

(define ((make-enum-writer ename fields) num name value out)
  (unless (hash-has-key? fields value)
    (error 'write-enum "no case named ~e~n  enum: ~a~n  field: ~a" value ename name))
  (write-proto-int32 num name (hash-ref fields value) out))


;; message ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (struct-out message)
 (struct-out message-field))

(struct message-field (name num flags reader writer))
(struct message (name options reader writer))

(define (make-message name children field-nodes option-nodes reserved _extensions parent-env)
  (define reserved? (make-reserved?-proc reserved))
  (define options (make-options option-nodes))
  (define env (make-env parent-env))
  (for ([node (in-list children)])
    (match node
      [(Enum _ name fields options reserved)
       (define e (make-enum name fields options reserved))
       (env-set! env name e)]
      [(Message _ name children fields options reserved extensions)
       (define m (make-message name children fields options reserved extensions env))
       (env-set! env name m)]
      [_
       (log-protobuf-warning "ignoring node ~e (child of message ~a)" node name)]))
  (define-values (fields required)
    (let loop ([fields (hasheq)]
               [required null]
               [nodes field-nodes])
      (for/fold ([fields fields]
                 [required required])
                ([node (in-list nodes)])
        (match node
          [(MessageField tok label type name number option-nodes)
           (when (reserved? (symbol->string name) number)
             (oops tok "field ~a is reserved" name))
           (define optional? (eq? label 'optional))
           (define repeated? (eq? label 'repeated))
           (define options (make-options option-nodes))
           (define packed? (and repeated?
                                (scalar-type? type)
                                (hash-ref options 'packed #f)))
           (define flags
             (fxior (if optional? optional-mask 0)
                    (if repeated? repeated-mask 0)
                    (if packed? packed-mask 0)))
           (define reader
             ((if packed?
                  get-packed-reader
                  get-reader)
              tok env type))
           (define writer
             ((cond
                [packed? get-packed-writer]
                [repeated? get-repeated-writer]
                [else get-writer]) tok env type))
           (define f (message-field name number flags reader writer))
           (values (hash-set fields name f)
                   (if optional? required (cons name required)))]
          [(MessageOneOfField _ _ field-nodes)
           (loop fields field-nodes)]
          [(MessageMapField tok key-type val-type name number _option-nodes)
           (when (reserved? (symbol->string name) number)
             (oops tok "field ~a is reserved" name))
           (define k-reader (get-reader tok env key-type))
           (define k-writer (get-writer tok env key-type))
           (define v-reader (get-reader tok env val-type))
           (define v-writer (get-writer tok env val-type))
           (define reader (make-map-reader k-reader v-reader))
           (define writer (make-map-writer k-writer v-writer))
           (define f (message-field name number 0 reader writer))
           (values (hash-set fields name f) required)]
          [_
           (log-protobuf-warning "ignoring field node ~e" node)
           (values fields required)]))))
  (define reader (make-message-reader name fields required))
  (define writer (make-message-writer name fields required))
  (message name options reader writer))

(define (make-message-reader mname fields required)
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
           (match-define (message-field name _number flags reader _writer)
             (hash-ref numbers-to-fields num (λ () (error 'read-message "no field with number ~a in message ~a" num mname))))
           (define v
             (reader tag in))
           (loop
            (remq name missing)
            (if (repeated? flags)
                (hash-update
                 res name
                 (λ (vs)
                   (if (pair? v)
                       (append (reverse v) vs)
                       (cons v vs)))
                 null)
                (hash-set res name v)))])))
    (begin0 res
      (unless (null? missing)
        (define fields-str (string-join (map symbol->string missing) ", "))
        (error 'read-message "missing required fields for message ~a: ~a" mname fields-str)))))

(define ((make-message-writer mname fields required) name value out)
  (unless (hash? value)
    (if name
        (error 'write-message "expected a hash~n  got: ~e~n  field: ~a" value name)
        (error 'write-message "expected a hash~n  got: ~e" value)))
  (define missing
    (for/fold ([missing required])
              ([(k v) (in-hash value)])
      (match-define (message-field name num _flags _reader writer)
        (hash-ref fields k (λ () (error 'write-message "unknown field ~a for message ~a" k mname))))
      (writer num name v out)
      (remq k missing)))
  (unless (null? missing)
    (define fields-str (string-join (map symbol->string missing) ", "))
    (error 'write-message "missing required fields for message ~a: ~a" mname fields-str)))

(define ((make-limited-reader who proc [result-proc values]) tag in)
  (define len (read-proto-len who tag in))
  (define limited-in (make-limited-input-port in len #f))
  (let loop ([res #f])
    (cond
      [(eof-object? (peek-byte limited-in))
       (result-proc res)]
      [else
       (loop (proc limited-in res))])))

(define (make-map-reader k-reader v-reader)
  (make-limited-reader
   'read-map
   (λ (in res)
     (define-values (k-num k-tag)
       (read-proto-tag in))
     (unless (eqv? k-num 1)
       (error 'read-map "malformed map key"))
     (define k (k-reader k-tag in))
     (define-values (v-num v-tag)
       (read-proto-tag in))
     (unless (eqv? v-num 2)
       (error 'read-map "malformed map value"))
     (define v (v-reader v-tag in))
     (hash-set (or res (hash)) k v))))

(define ((make-map-writer k-writer v-writer) num name value out)
  (unless (hash? value)
    (error 'write-map "expected a hash~n  got: ~e~n  field: ~a" value name))
  (define bs
    (call-with-output-bytes
     (lambda (bs-out)
       (define k-field (format "key of ~a" name))
       (define v-field (format "value of ~a" name))
       (for ([(k v) (in-hash value)])
         (k-writer 1 k-field k bs-out)
         (v-writer 2 v-field v bs-out)))))
  (write-proto-bytes num name bs out))

(define (get-packed-reader tok e t)
  (define reader
    (get-reader tok e t))
  (make-limited-reader
   'read-packed
   (λ (in res)
     (cons (reader #f in) (or res null)))
   (λ (res)
     (reverse res))))

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

(define (get-packed-writer tok e t)
  (define writer
    (get-writer tok e t))
  (λ (num name value out)
    (unless (list? value)
      (error 'write-packed "expected a list~n  got: ~e~n  field: ~a" value name))
    (define bs
      (call-with-output-bytes
       (lambda (bs-out)
         (for ([v (in-list value)])
           (writer #f name v bs-out)))))
    (write-proto-bytes num name bs out)))

(define (get-repeated-writer tok e t)
  (define writer
    (get-writer tok e t))
  (λ (num name value out)
    (unless (list? value)
      (error 'write-repeated "expected a list~n  got: ~e~n  field: ~a" value name))
    (for ([v (in-list value)])
      (writer num name v out))))

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


;; flags ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define optional-mask #b00000001)
(define repeated-mask #b00000010)
(define packed-mask   #b00000100)

(define (repeated? f)
  (flag-on? f repeated-mask))


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-options option-nodes)
  (for/hash ([node (in-list option-nodes)])
    (match node
      [(Option _ (list name) value)
       (values name value)]
      [(Option _ path value)
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
      [(cons (Range _ lo hi) reserved-rest)
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

(define (flag-on? n m)
  (fx= (fxand n m) m))
