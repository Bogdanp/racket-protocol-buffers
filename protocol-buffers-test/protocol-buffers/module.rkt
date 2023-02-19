#lang racket/base

(require protocol-buffers
         (prefix-in private: protocol-buffers/private/module)
         (prefix-in private: protocol-buffers/private/write)
         racket/port
         rackunit)

(define (read-protobuf-str s)
  (call-with-input-string s read-protobuf))

(define (roundtrip m v)
  (define bs
    (call-with-output-bytes
     (lambda (out)
       (write-message m v out))))
  (call-with-input-bytes bs
    (lambda (in)
      (read-message m in))))

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
                             ))))))

   (test-suite
    "options"
    (test-case "toplevel"
      (define m (read-protobuf-str #<<MOD
syntax = 'proto3';

option a = 1;
option (b).c = 2;
MOD
                                   ))
      (check-equal?
       (mod-options m)
       (hash 'a 1
             '(b c) 2)))

    (test-case "enum"
      (define  m (read-protobuf-str #<<MOD
syntax = 'proto3';

enum A {
  option alias = true;
  option (java).example = "test";
  FOO = 0;
}
MOD
                                    ))
      (check-equal?
       (private:enum-options (car (private:mod-types m)))
       (hash 'alias #t
             '(java example) "test"))))

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
     (check-equal? (roundtrip (mod-ref m 'A) d) d))))

(module+ test
  (require rackunit/text-ui)
  (run-tests module-suite))
