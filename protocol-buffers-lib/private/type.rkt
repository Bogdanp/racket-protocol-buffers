#lang racket/base

(provide
 scalar-type?
 primitive-type?)

(define (scalar-type? t)
  (and (memq t '(bool double fixed32 fixed64 float int32 int64 sfixed32 sfixed64 sint32 sint64 uint32 uint64)) #t))

(define (primitive-type? t)
  (or (scalar-type? t)
      (and (memq t '(bytes string)))))
