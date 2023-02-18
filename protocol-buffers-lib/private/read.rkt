#lang racket/base

(require racket/fixnum
         "varint.rkt")

(provide
 read-proto-tag
 read-proto-len
 read-proto-bool
 read-proto-string
 read-proto-bytes
 read-proto-int32
 read-proto-int64
 read-proto-uint32
 read-proto-uint64
 read-proto-sint32
 read-proto-sint64
 read-proto-fixed32
 read-proto-fixed64
 read-proto-sfixed32
 read-proto-sfixed64
 read-proto-double
 read-proto-float)

(define (read-proto-tag in)
  (define v (read-uvarint in))
  (define t (fxand v #b111))
  (define tag
    (case t
      [(0) 'varint]
      [(1) 'i64]
      [(2) 'len]
      [(3) 'sgroup]
      [(4) 'egroup]
      [(5) 'i32]
      [else (error 'read-proto-tag "unexpected tag: ~a" t)]))
  (values (fxrshift v 3) tag))

(define (read-proto-len who tag in)
  (check-tag 'read-proto-len "len" tag 'len)
  (read-uvarint in))

(define (read-proto-bool tag in)
  (read-proto-int32 tag in))

(define (read-proto-string tag in)
  (bytes->string/utf-8 (read-proto-bytes tag in)))

(define (read-proto-bytes tag in)
  (expect-bytes 'bytes (read-proto-len 'bytes tag in) in))

(define (read-proto-int32 tag in)
  (check-tag 'pread-proto-int32 "int32" tag 'varint)
  (define v
    (read-uvarint in))
  (if (> v #x7FFFFFFF)
      (- v #xFFFFFFFF 1)
      v))

(define (read-proto-int64 tag in)
  (check-tag 'read-proto-int64 "int64" tag 'varint)
  (define v
    (read-uvarint in))
  (if (> v #x7FFFFFFFFFFFFFFF)
      (- v #xFFFFFFFFFFFFFFFF 1)
      v))

(define (read-proto-uint32 tag in)
  (check-tag 'read-proto-uint32 "uint32" tag 'varint)
  (read-uvarint in))

(define (read-proto-uint64 tag in)
  (check-tag 'read-proto-uint64 "uint64" tag 'varint)
  (read-uvarint in))

(define (read-proto-sint32 tag in)
  (check-tag 'read-proto-sint32 "sint32" tag 'varint)
  (read-varint in))

(define (read-proto-sint64 tag in)
  (check-tag 'read-proto-sint64 "sint64" tag 'varint)
  (read-varint in))

(define (read-proto-fixed32 tag in)
  (check-tag 'read-proto-fixed32 "fixed32" tag 'i32)
  (integer-bytes->integer (expect-bytes 'fixed32 4 in) #f #f))

(define (read-proto-fixed64 tag in)
  (check-tag 'read-proto-fixed64 "fixed64" tag 'i64)
  (integer-bytes->integer (expect-bytes 'fixed64 8 in) #f #f))

(define (read-proto-sfixed32 tag in)
  (check-tag 'read-proto-sfixed32 "sfixed32" tag 'i32)
  (integer-bytes->integer (expect-bytes 'sfixed32 4 in) #t #f))

(define (read-proto-sfixed64 tag in)
  (check-tag 'read-proto-sfixed64 "sfixed64" tag 'i64)
  (integer-bytes->integer (expect-bytes 'sfixed64 8 in) #t #f))

(define (read-proto-double tag in)
  (check-tag 'read-proto-float "double" tag 'i64)
  (floating-point-bytes->real (expect-bytes 'double 8 in) #f))

(define (read-proto-float tag in)
  (check-tag 'read-proto-float "float" tag 'i32)
  (floating-point-bytes->real (expect-bytes 'float 4 in) #f))

;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (check-tag who what tag expected)
  (when tag
    (unless (eq? tag expected)
      (error who "unexpected tag for ~a: ~a" what tag))))

(define (expect-bytes what amt in)
  (define bs (read-bytes amt in))
  (begin0 bs
    (unless (and (bytes? bs) (= (bytes-length bs) amt))
      (error 'expect-bytes "unexpected EOF while reading ~a" what))))
