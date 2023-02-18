#lang racket/base

(require racket/fixnum
         "varint.rkt")

(provide
 write-proto-tag
 write-proto-bool
 write-proto-string
 write-proto-bytes)

(define (write-proto-tag num tag out)
  (write-uvarint (fxior (fxlshift num 3) tag) out))

(define (write-proto-bool num _name value out)
  (write-proto-tag num 0 out)
  (write-uvarint (if value 1 0) out))

(define (write-proto-string num name value out)
  (unless (string? value)
    (error 'write-proto-string "expected a string~n  got: ~e~n  field: ~a" value name))
  (write-proto-bytes num name (string->bytes/utf-8 value) out))

(define (write-proto-bytes num name value out)
  (unless (bytes? value)
    (error 'write-proto-bytes "expected bytes~n  got: ~e~n  field: ~a" value name))
  (write-proto-tag num 2 out)
  (write-uvarint (bytes-length value) out)
  (write-bytes value out))

(define-syntax-rule (define-int-writer (id value-id out-id) #:range [lo hi] write-expr)
  (begin
    (provide id)
    (define (id num name value-id out-id)
      (unless (integer-in-range? value-id lo hi)
        (error 'id "integer out of range~n  got: ~e~n  field: ~a" value-id name))
      (write-proto-tag num 0 out-id)
      write-expr)))

(define-int-writer (write-proto-int32 value out)
  #:range [#x-80000000 #x7FFFFFFF]
  (write-uvarint (if (< value 0) (+ #xFFFFFFFF value 1) value) out))

(define-int-writer (write-proto-int64 value out)
  #:range [#x-8000000000000000 #x7FFFFFFFFFFFFFFF]
  (write-uvarint (if (< value 0) (+ #xFFFFFFFFFFFFFFFF value 1) value) out))

(define-int-writer (write-proto-uint32 value out)
  #:range [0 #xFFFFFFFF]
  (write-uvarint value out))

(define-int-writer (write-proto-uint64 value out)
  #:range [0 #xFFFFFFFFFFFFFFFF]
  (write-uvarint value out))

(define-int-writer (write-proto-sint32 value out)
  #:range [#x-80000000 #x7FFFFFFF]
  (write-varint value out))

(define-int-writer (write-proto-sint64 value out)
  #:range [#x-8000000000000000 #x7FFFFFFFFFFFFFFF]
  (write-varint value out))

(define-syntax-rule (define-fixed-writer id #:signed? signed?-expr #:tag tag-expr #:size size-expr)
  (begin
    (provide id)
    (define (id num name value out)
      (unless (exact-integer? value)
        (error 'id "expected an integer~n  got: ~e~n  field: ~a" value name))
      (write-proto-tag num tag-expr out)
      (write-bytes (integer->integer-bytes value size-expr signed?-expr #f) out))))

(define-fixed-writer write-proto-fixed32 #:signed? #f #:tag 5 #:size 4)
(define-fixed-writer write-proto-fixed64 #:signed? #f #:tag 1 #:size 8)

(define-fixed-writer write-proto-sfixed32 #:signed? #f #:tag 5 #:size 4)
(define-fixed-writer write-proto-sfixed64 #:signed? #t #:tag 1 #:size 8)

(define-syntax-rule (define-real-writer id #:tag tag-expr #:size size-expr)
  (begin
    (provide id)
    (define (id num name value out)
      (unless (real? value)
        (error 'id "expected a real number~n  got: ~e~n  field: ~a" value name))
      (write-proto-tag num tag-expr out)
      (write-bytes (real->floating-point-bytes value size-expr #f) out))))

(define-real-writer write-proto-double #:tag 1 #:size 8)
(define-real-writer write-proto-float  #:tag 5 #:size 4)


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (integer-in-range? v lo hi)
  (and (exact-integer? v) (>= v lo) (<= v hi)))
