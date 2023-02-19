#lang racket/base

(require protocol-buffers
         racket/pretty
         racket/runtime-path)

(define-runtime-path here ".")

(define mod
  (parameterize ([current-directory here])
    (call-with-input-file (build-path here "spec.proto")
      read-protobuf)))

(define People (mod-ref mod 'People))
(define people
  (hasheq
   'people (list
            (hasheq
             'id 1
             'name "Bogdan"
             'age 30
             'sex 'MALE
             'relationships (hash 2 'SIBLING))
            (hasheq
             'id 2
             'name "Paula"
             'age 23
             'sex 'FEMALE
             'relationships (hash 1 'SIBLING)))))

(call-with-output-file (build-path here "example.dat")
  #:exists 'replace
  (lambda (out)
    (write-message People people out)))

(pretty-print
 (call-with-input-file (build-path here "example.dat")
   (lambda (in)
     (read-message People in))))
