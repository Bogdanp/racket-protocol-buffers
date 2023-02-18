#lang racket/base

(require protobuf
         racket/runtime-path)

(define-runtime-path here ".")

(define mod
  (parameterize ([current-directory here])
    (call-with-input-file (build-path here "spec.proto")
      read-protobuf-mod)))

(define Outer
  (car (mod-messages mod)))

(define data
  (hasheq 'enum_field 'EAA_STARTED
          'my_map (hash 5 "hello")
          'inner_message (list (hasheq 'ival -1024))))

(call-with-output-file (build-path here "example.dat")
  #:exists 'replace
  (lambda (out)
    (write-message Outer data out)))

(equal? data
        (call-with-input-file (build-path here "example.dat")
          (lambda (in)
            (read-message Outer in))))
