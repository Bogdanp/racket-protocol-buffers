#lang racket/base

(require protocol-buffers
         rackcheck
         rackcheck/shrink-tree
         racket/file
         racket/list
         racket/match
         racket/port
         racket/string
         racket/system
         "common.rkt")

(define missing (gensym 'missing))

(define (gen:seq [init-val 0])
  (let ([seq init-val])
    (make-gen
     (lambda (_prng _size)
       (begin0 (make-shrink-tree seq)
         (set! seq (add1 seq)))))))

(define gen:bool
  (gen:map
   gen:boolean
   (lambda (b)
     (if b #t missing))))

(define gen:int
  (gen:map
   (gen:choice
    (gen:integer-in #x-80000000 #x0)
    (gen:integer-in #x0 #x7FFFFFFF))
   (lambda (n)
     (if (zero? n) missing n))))

(define gen:uint
  (gen:map
   (gen:choice
    (gen:integer-in #x0 #x7FFFFFFF)
    (gen:integer-in #x80000000 #xFFFFFFFF))
   (lambda (n)
     (if (zero? n) missing n))))

(define gen:primitive-type
  (gen:choice
   (gen:const (cons 'bool gen:bool))
   (gen:const (cons 'bytes (gen:map
                            (gen:bytes)
                            (lambda (bs)
                              (if (bytes=? #"") missing bs)))))
   (gen:const (cons 'string (gen:map
                             (gen:string)
                             (lambda (s)
                               (if (string=? s "") missing s)))))
   (gen:const (cons 'int32 gen:int))
   (gen:const (cons 'int64 gen:int))
   (gen:const (cons 'uint32 gen:uint))
   (gen:const (cons 'uint64 gen:uint))
   (gen:const (cons 'sint32 gen:int))
   (gen:const (cons 'sint64 gen:int))
   (gen:const (cons 'fixed32 gen:uint))
   (gen:const (cons 'fixed64 gen:uint))
   (gen:const (cons 'sfixed32 gen:int))
   (gen:const (cons 'sfixed64 gen:int))
   (gen:const (cons 'double gen:real))
   (gen:const (cons 'float (gen:const 3.140000104904175)))))

(define gen:name-str
  (let ([gen:name-seq (gen:seq)])
    (gen:let ([start gen:char-letter]
              [rest (gen:list gen:char-alphanumeric #:max-length 10)]
              [seq gen:name-seq])
      (string-append
       (apply string start rest)
       (number->string seq)))))

(define gen:type-name
  (gen:map gen:name-str (compose1 string->symbol string-titlecase)))
(define gen:field-name
  (gen:map gen:name-str (compose1 string->symbol string-downcase)))

(define (gen:enum generators)
  (gen:let ([name gen:type-name]
            [field-nums (gen:list (gen:seq 1) #:max-length 10)]
            [all-field-nums (gen:const (cons 0 field-nums))]
            [all-field-names (apply gen:tuple (make-list (length all-field-nums) gen:field-name))])
    (define fields-str
      (string-join
       (for/list ([num (in-list all-field-nums)]
                  [name (in-list all-field-names)])
         (format "  ~a = ~a;" name num))
       "\n"))
    (define str
      (format #<<SRC
enum ~a {
~a
}
SRC
              name
              fields-str))
    (hash-set! generators name (gen:map
                                (gen:one-of all-field-names)
                                (lambda (name)
                                  (if (eq? name (car all-field-names)) missing name))))
    (cons name str)))

(define (gen:message version name all-names generators)
  (gen:let ([field-nums (gen:list (gen:seq 2) #:max-length 10)]
            [all-field-nums (gen:const (cons 1 field-nums))]
            [all-field-names (apply gen:tuple (make-list (length all-field-nums) gen:field-name))]
            [all-field-types (apply gen:tuple (make-list (length all-field-nums) (gen:choice
                                                                                  gen:primitive-type
                                                                                  (gen:one-of all-names))))])
    (define fields
      (for/list ([num (in-list all-field-nums)]
                 [name (in-list all-field-names)]
                 [type (in-list all-field-types)])
        (list num name type)))
    (define fields-str
      (string-join
       (for/list ([f (in-list fields)])
         (match-define (list num name type) f)
         (define type* (if (symbol? type) type (car type)))
         (define label (if (eq? version 'proto2) "optional" ""))
         (format "  ~a ~a ~a = ~a;~n" label type* name num))))
    (define str
      (format "message ~a { ~a }" name fields-str))
    (hash-set! generators name (gen:map
                                (apply
                                 gen:hasheq
                                 (flatten
                                  (for/list ([f (in-list fields)])
                                    (match-define (list _num name type) f)
                                    (cond
                                      [(symbol? type)
                                       (list name (gen:frequency
                                                   `((10 . ,(gen:const missing))
                                                     (1 . ,(gen:delay (hash-ref generators type))))))]
                                      [else
                                       (list name (cdr type))]))))
                                (λ (ht)
                                  (for/hasheq ([(k v) (in-hash ht)] #:unless (eq? v missing))
                                    (values k v)))))
    (cons name str)))

(define gen:module
  (gen:let ([generators (gen:const (make-hasheq))]
            [version (gen:one-of '(proto2 proto3))]
            [enums (gen:list (gen:enum generators) #:max-length 5)]
            [first-message-name gen:type-name]
            [message-names (gen:list gen:type-name #:max-length 10)]
            [all-message-names (gen:const (cons first-message-name message-names))]
            [messages (apply gen:tuple (for/list ([name (in-list all-message-names)])
                                         (gen:message version name (append all-message-names (map car enums)) generators)))])
    (define enums-str
      (string-join (map cdr enums) "\n"))
    (define messages-str
      (string-join (map cdr messages) "\n"))
    (define str
      (format "syntax = '~a';~n~a~n~a" version enums-str messages-str))
    (define msg
      (car all-message-names))
    (list str msg (hash-ref generators msg))))

(module+ test
  (require racket/runtime-path
           rackunit)

  (define-property internal-roundtrip
    ([m gen:module])
    (match-define (list str msg-id gen:value) m)
    (define mod (call-with-input-string str read-protobuf))
    (define msg (mod-ref mod msg-id))
    (with-handlers ([exn:fail? (λ (e) (error (exn-message e)))])
      (check-property
       (property ([v gen:value])
         (define roundtripped-v (roundtrip msg v))
         (check-equal? roundtripped-v v)))))

  (check-property
   (make-config #:tests 100)
   internal-roundtrip)

  (define protoc (find-executable-path "protoc"))
  (define python (find-executable-path "python"))

  (define-runtime-path here ".")
  (define oracle-dir (build-path here "oracle"))
  (define oracle-init.py (build-path oracle-dir "__init__.py"))
  (define oracle.py (build-path here "oracle.py"))
  (define-property external-roundtrip
    ([m gen:module])
    (match-define (list str msg-id gen:value) m)
    (define mod (call-with-input-string str read-protobuf))
    (define msg (mod-ref mod msg-id))
    (define path (make-temporary-file "~a.proto"))
    (define-values (dir name _is_dir?)
      (split-path path))
    (call-with-output-file path
      #:exists 'replace
      (lambda (out)
        (displayln str out)))
    (delete-directory/files oracle-dir #:must-exist? #f)
    (make-directory* oracle-dir)
    (call-with-output-file oracle-init.py void)
    (check-true (zero? (system*/exit-code protoc "-I" dir name "--python_out" oracle-dir)))
    (with-handlers ([exn:fail? (λ (e) (error (exn-message e)))])
      (check-property
       (make-config #:tests 10)
       (property ([v gen:value])
         (define v-bs
           (call-with-output-bytes
            (lambda (out)
              (write-message msg v out))))
         (match-define (list python-in python-out _python-pid python-err control)
           (process* python
                     oracle.py
                     (path->bytes (path-replace-extension name #""))
                     (symbol->string msg-id)))
         (write-bytes v-bs python-out)
         (close-output-port python-out)
         (control 'wait)
         (define roundtripped-v
           (read-message msg python-in))
         (define error-str
           (port->string python-err))
         (unless (zero? (control 'exit-code))
           (displayln error-str)
           (error 'python error-str))
         (close-input-port python-in)
         (close-input-port python-err)
         (check-equal? roundtripped-v v)))))

  (when protoc
    (check-property
     (make-config #:tests 100)
     external-roundtrip)))
