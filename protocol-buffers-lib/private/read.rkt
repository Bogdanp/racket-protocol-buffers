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
  (unless (eq? tag 'len)
    (error who "unexpected tag for ~a: ~a" who tag))
  (read-uvarint in))

(define (read-proto-bool tag in)
  (unless (eq? tag 'varint)
    (error 'read-proto-bool "unexpected tag for bool: ~a" tag))
  (read-proto-int32 in))

(define (read-proto-string tag in)
  (bytes->string/utf-8 (read-proto-bytes tag in)))

(define (read-proto-bytes tag in)
  (expect-bytes 'bytes (read-proto-len 'bytes tag in) in))

(define (read-proto-int32 tag in)
  (unless (eq? tag 'varint)
    (error 'read-proto-int32 "unexpected tag for int32: ~a" tag))
  (define v
    (read-uvarint in))
  (if (> v #x7FFFFFFF)
      (- v #xFFFFFFFF 1)
      v))

(define (read-proto-int64 tag in)
  (unless (eq? tag 'varint)
    (error 'read-proto-int64 "unexpected tag for int64: ~a" tag))
  (define v
    (read-uvarint in))
  (if (> v #x7FFFFFFFFFFFFFFF)
      (- v #xFFFFFFFFFFFFFFFF 1)
      v))

(define (read-proto-uint32 tag in)
  (unless (eq? tag 'varint)
    (error 'read-proto-uint32 "unexpected tag for uint32: ~a" tag))
  (read-uvarint in))

(define (read-proto-uint64 tag in)
  (unless (eq? tag 'varint)
    (error 'read-proto-uint64 "unexpected tag for uint64: ~a" tag))
  (read-uvarint in))

(define (read-proto-sint32 tag in)
  (unless (eq? tag 'varint)
    (error 'read-proto-sint32 "unexpected tag for sint32: ~a" tag))
  (read-varint in))

(define (read-proto-sint64 tag in)
  (unless (eq? tag 'varint)
    (error 'read-proto-sint64 "unexpected tag for sint64: ~a" tag))
  (read-varint in))

(define (read-proto-fixed32 tag in)
  (unless (eq? tag 'i32)
    (error 'read-proto-fixed32 "unexpected tag for fixed32: ~a" tag))
  (integer-bytes->integer (expect-bytes 'fixed32 4 in) #f #f))

(define (read-proto-fixed64 tag in)
  (unless (eq? tag 'i64)
    (error 'read-proto-fixed64 "unexpected tag for fixed64: ~a" tag))
  (integer-bytes->integer (expect-bytes 'fixed64 8 in) #f #f))

(define (read-proto-sfixed32 tag in)
  (unless (eq? tag 'i32)
    (error 'read-proto-sfixed32 "unexpected tag for sfixed32: ~a" tag))
  (integer-bytes->integer (expect-bytes 'sfixed32 4 in) #t #f))

(define (read-proto-sfixed64 tag in)
  (unless (eq? tag 'i64)
    (error 'read-proto-sfixed64 "unexpected tag for sfixed64: ~a" tag))
  (integer-bytes->integer (expect-bytes 'sfixed64 8 in) #t #f))

(define (read-proto-double tag in)
  (unless (eq? tag 'i64)
    (error 'read-proto-double "unexpected tag for double: ~a" tag))
  (floating-point-bytes->real (expect-bytes 'double 8 in) #f))

(define (read-proto-float tag in)
  (unless (eq? tag 'i32)
    (error 'read-proto-float "unexpected tag for float: ~a" tag))
  (floating-point-bytes->real (expect-bytes 'float 4 in) #f))

;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (expect-bytes what amt in)
  (define bs (read-bytes amt in))
  (begin0 bs
    (unless (and (bytes? bs) (= (bytes-length bs) amt))
      (error 'expect-bytes "unexpected EOF while reading ~a" what))))
