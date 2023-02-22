#lang racket/base

(require protocol-buffers
         (prefix-in private: protocol-buffers/private/write)
         racket/port
         racket/runtime-path
         rackunit
         "common.rkt")

(define (read-protobuf-str s)
  (call-with-input-string s read-protobuf))

(define-runtime-path examples "examples")

(define module-suite
  (test-suite
   "module"

   (test-suite
    "validation"

    (test-case "multiple package declarations"
      (check-exn
       #rx"package already declared"
       (λ ()
         (read-protobuf-str #<<MOD
syntax = 'proto3';
package a;
package b;
MOD
                            ))))


    (test-suite
     "enum"

     (test-case "reserved fields"
       (check-exn
        #rx"field B is reserved"
        (λ ()
          (read-protobuf-str #<<MOD
syntax = 'proto2';

enum Foo {
  A = 1;
  B = 5;
  reserved "C", 3, 5 to 10;
}
MOD
                             ))))

     (test-case "field values"
       (check-exn
        #rx"expected number but found 'hello'"
        (λ ()
          (read-protobuf-str #<<MOD
syntax = 'proto2';

enum Foo {
  A = 'hello';
}
MOD
                             )))

       (check-exn
        #rx"expected an integer"
        (λ ()
          (read-protobuf-str #<<MOD
syntax = 'proto2';

enum Foo {
  A = 2.5;
}
MOD
                             )))

       (check-exn
        #rx"the first enum value must be zero in proto3"
        (λ ()
          (read-protobuf-str #<<MOD
syntax = 'proto3';

enum Foo {
  X = 1;
}
MOD
                             )))))

    (test-suite
     "message"

     (test-case "group fields"
       (check-exn
        #rx"group fields are not supported"
        (λ ()
          (read-protobuf-str #<<MOD
syntax = 'proto2';

message Foo {
  repeated group Result = 1 {
    required string a = 2;
    required int32 b = 3;
  }
}
MOD
                             ))))

     (test-case "default values"
       (check-exn
        #rx"invalid default value for field"
        (λ ()
          (read-protobuf-str #<<MOD
syntax = 'proto2';

message Foo {
  required int32 x = 1 [default = 'a'];
}
MOD
                             )))

       (check-exn
        #rx"message fields can't have default values"
        (λ ()
          (read-protobuf-str #<<MOD
syntax = 'proto2';

message Foo {
  optional Foo f = 1 [default = 42];
}
MOD
                             )))

       (check-exn
        #rx"maps can't have default values"
        (λ ()
          (read-protobuf-str #<<MOD
syntax = 'proto2';

message Foo {
  map<int32, int32> m = 1 [default = 42];
}
MOD
                             )))


       (check-exn
        #rx"explicit default values are not allowed in proto3"
        (λ ()
          (read-protobuf-str #<<MOD
syntax = 'proto3';

message Foo {
  int32 x = 1 [default = 42];
}
MOD
                             ))))))

   (test-case "packed option"
     (define m (read-protobuf-str #<<MOD
syntax = 'proto2';

message Example {
  repeated int32 x = 1 [packed = true];
}
MOD
                                  ))
     (define Example (mod-ref m 'Example))

     (test-case "roundtrip empty"
       (define d (hasheq 'x null))
       (check-equal? (roundtrip Example d) d))

     (test-case "roundtrip list"
       (define d (hasheq 'x '(1 -1 2 #x7FFFFFFF)))
       (check-equal? (roundtrip Example d) d))

     (test-case "read split"
       (define bs
         (call-with-output-bytes
          (lambda (out)
            ;; 1: {1}
            ;; 1: {2 3}
            ;; 1: {4 5 6}
            ;; 1: {7}
            (define (make-packed xs)
              (call-with-output-bytes
               (lambda (bs-out)
                 (for ([x (in-list xs)])
                   (private:write-proto-int32 #f #f x bs-out)))))
            (private:write-proto-bytes 1 'x (make-packed '(1)) out)
            (private:write-proto-bytes 1 'x (make-packed '(2 3)) out)
            (private:write-proto-bytes 1 'x (make-packed '(4 5 6)) out)
            (private:write-proto-bytes 1 'x (make-packed '(7)) out))))
       (check-equal?
        (read-message Example (open-input-bytes bs))
        (hasheq
         'x '(1 2 3 4 5 6 7)))))

   (test-case "order independence"
     (define m (read-protobuf-str #<<MOD
syntax = 'proto2';

message A {
  required B b = 1;
}

message B {
  required string s = 1;
}
MOD
                                  ))
     (define d (hasheq 'b (hasheq 's "hello")))
     (check-equal? (roundtrip (mod-ref m 'A) d) d))

   (test-suite
    "import"

    (test-case "import proto3 from proto2"
      (define m
        (parameterize ([current-directory examples])
          (read-protobuf-str #<<MOD
syntax = 'proto2';

import public "person.proto";

message AddressBook {
  repeated Person people = 1;
}
MOD
                             )))
      (check-not-false (mod-ref m 'Person))

      (define AddressBook
        (mod-ref m 'AddressBook))
      (check-not-false AddressBook)

      (define book
        (hasheq
         'people (list
                  (hasheq 'name "Bogdan"
                          'id 1
                          'email "bogdan@defn.io"
                          'phones (list
                                   (hasheq 'number "55512341234" 'type 'WORK)
                                   (hasheq 'number "55512041204" 'type 'HOME))))))
      (check-equal? (roundtrip AddressBook book) book))

    (test-case "private import"
      (define m
        (parameterize ([current-directory examples])
          (read-protobuf-str #<<MOD
syntax = 'proto2';

import "person.proto";
MOD
                             )))
      (check-false (mod-ref m 'Person (λ () #f)))))

   (test-suite
    "unsupported features"

    (test-suite
     "extensions"

     (test-case "top-level"
       (check-exn
        #rx"extensions are not supported"
        (λ ()
          (read-protobuf-str #<<MOD
syntax = 'proto2';

message Foo {
  extensions 100 to 999;
}

extend Foo {
  optional string s = 100;
}
MOD
                             ))))

     (test-case "nested"
       (check-exn
        #rx"extensions are not supported"
        (λ ()
          (read-protobuf-str #<<MOD
syntax = 'proto2';

message Foo {
  extensions 100 to 999;
}

message Bar {
  extend Foo {
    optional string s = 100;
  }

  optional Foo f = 1;
}
MOD
                             ))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests module-suite))
