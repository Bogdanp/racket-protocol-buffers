#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         protocol-buffers
         racket/file
         racket/match
         racket/port
         racket/runtime-path
         racket/system
         rackunit
         (only-in rackunit/private/check-info location-info))

(define protoc (find-executable-path "protoc"))
(define python (find-executable-path "python"))

(define-runtime-path here ".")
(define oracle-dir (build-path here "oracle"))
(define oracle/__init__.py (build-path oracle-dir "__init__.py"))
(define oracle.py (build-path here "oracle.py"))

(define (compile-proto proto)
  (define path (make-temporary-file "~a.proto"))
  (define-values (dir-path name _dir?)
    (split-path path))
  (call-with-output-file path
    #:exists 'replace
    (lambda (out)
      (displayln proto out)))
  (delete-directory/files oracle-dir #:must-exist? #f)
  (make-directory* oracle-dir)
  (call-with-output-file oracle/__init__.py void)
  (unless (zero? (system*/exit-code protoc "-I" dir-path name "--python_out" oracle-dir))
    (error 'proto-compile "protoc failed~n  proto: ~a" proto))
  name)

(define (roundtrip-via-oracle msg name v)
  (match-define (list in out _pid err control)
    (process* python
              oracle.py
              (path->string (path-replace-extension name #""))
              "Test"))
  (write-message msg v out)
  (close-output-port out)
  (control 'wait)
  (begin0 (read-message msg in)
    (let ([error-str (port->string err)])
      (close-input-port in)
      (close-input-port err)
      (unless (zero? (control 'exit-code))
        (error 'python error-str)))))

(begin-for-syntax
  (define (syntax->location-stx stx)
    (datum->syntax
     stx
     (list
      'location-info
      (list
       'list
       (syntax-source stx)
       (syntax-line stx)
       (syntax-column stx)
       (syntax-position stx)
       (syntax-span stx))))))

(define-syntax (check-roundtrip stx)
  (syntax-parse stx
    [(_ proto-str:string [in {~optional out}] ...+)
     #:with mod #'mod
     #:with msg #'msg
     #:with name #'name
     #:with (check ...)
     (for/list ([in-stx (in-list (syntax-e #'(in ...)))]
                [out-stx (in-list (syntax-e #'({~? out in} ...)))])
       (with-syntax ([in in-stx]
                     [out out-stx])
         #`(let ([res (roundtrip-via-oracle msg name in)]
                 [exp out])
             (with-check-info (['location #,(syntax->location-stx out-stx)])
               (check-equal? res exp)))))
     #'(let ([proto proto-str])
         (define mod (read-protobuf (open-input-string proto)))
         (define msg (mod-ref mod 'Test))
         (define name (compile-proto proto))
         check ...)]))

(define oracle-suite
  (test-suite
   "oracle"

   (check-roundtrip
    #<<MOD
syntax = 'proto2';

message Test {
}
MOD
    [(hasheq)
     (hasheq)])

   (let ([ht (λ (k v)
               (hash-set
                (hasheq 'i32 0 'i64 0 'u32  0 'u64  0 's32 0   's64 0
                        'f32 0 'f64 0 'sf32 0 'sf64 0 'd   0.0 'f   0.0
                        'b #f)
                k v))])
     (check-roundtrip
      #<<MOD
syntax = 'proto3';

message Test {
  int32 i32 = 1;
  int64 i64 = 2;
  uint32 u32 = 3;
  uint64 u64 = 4;
  sint32 s32 = 5;
  sint64 s64 = 6;
  fixed32 f32 = 7;
  fixed64 f64 = 8;
  sfixed32 sf32 = 9;
  sfixed64 sf64 = 10;
  double d = 11;
  float f = 12;
  bool b = 13;
}
MOD
      [(hasheq)
       (ht 'i32 0)]
      [(hasheq 'i32 #x-80000000)
       (ht 'i32 #x-80000000)]
      [(hasheq 'i32 #x7FFFFFFF)
       (ht 'i32 #x7FFFFFFF)]
      [(hasheq 'i64 #x-800000000000000)
       (ht 'i64 #x-800000000000000)]
      [(hasheq 'i64 #x7FFFFFFFFFFFFFFF)
       (ht 'i64 #x7FFFFFFFFFFFFFFF )]
      [(hasheq 'u32 #xFFFFFFFF)
       (ht 'u32 #xFFFFFFFF)]
      [(hasheq 'u64 #xFFFFFFFFFFFFFFFF)
       (ht 'u64 #xFFFFFFFFFFFFFFFF)]
      [(hasheq 's32 #x-80000000)
       (ht 's32 #x-80000000)]
      [(hasheq 's32 #x7FFFFFFF)
       (ht 's32 #x7FFFFFFF)]
      [(hasheq 's64 #x-800000000000000)
       (ht 's64 #x-800000000000000)]
      [(hasheq 's64 #x7FFFFFFFFFFFFFFF)
       (ht 's64 #x7FFFFFFFFFFFFFFF)]
      [(hasheq 'f32 #xFFFFFFFF)
       (ht 'f32 #xFFFFFFFF)]
      [(hasheq 'f64 #xFFFFFFFFFFFFFFFF)
       (ht 'f64 #xFFFFFFFFFFFFFFFF)]
      [(hasheq 'sf32 #x7FFFFFFF)
       (ht 'sf32 #x7FFFFFFF)]
      [(hasheq 'sf64 #x7FFFFFFFFFFFFFFF)
       (ht 'sf64 #x7FFFFFFFFFFFFFFF)]
      [(hasheq 'd 3.14)
       (ht 'd 3.14)]
      [(hasheq 'f 3.14)
       (ht 'f 3.140000104904175)]
      [(hasheq 'b #t)
       (ht 'b #t)]))

   (check-roundtrip
    #<<MOD
syntax = 'proto2';

enum E {
  first = 1;
  second = 2;
}

message Test {
  required E e = 1;
}
MOD
    [(hasheq 'e 'first)]
    [(hasheq 'e 'second)])

   (check-roundtrip
    #<<MOD
syntax = 'proto2';

enum E {
  first = 5;
  second = 10;
}

message Test {
  optional E e = 1;
}
MOD
    [(hasheq)
     (hasheq 'e 'first)]
    [(hasheq 'e 'second)
     (hasheq 'e 'second)])

   (check-roundtrip
    #<<MOD
syntax = 'proto3';

message Test {
  int32 v = 1;
  Test next = 2;
}
MOD
    [(hasheq)
     (hasheq 'v 0 'next #f)]
    [(hasheq 'v 1)
     (hasheq 'v 1 'next #f)]
    [(hasheq 'v 1 'next (hasheq 'v 2))
     (hasheq 'v 1 'next (hasheq 'v 2 'next #f))])

   (check-roundtrip
    #<<MOD
syntax = 'proto2';

message Test {
  repeated int32 x = 1;
}
MOD
    [(hasheq)
     (hasheq 'x null)]
    [(hasheq 'x '(1 2 3))])

   (check-roundtrip
    #<<MOD
syntax = 'proto2';

message Test {
  repeated int32 x = 1 [packed = true];
}
MOD
    [(hasheq)
     (hasheq 'x null)]
    [(hasheq 'x '(1 2 3))])

   (check-roundtrip
    #<<MOD
syntax = 'proto3';

message Test {
  repeated int32 x = 1;
}
MOD
    [(hasheq)
     (hasheq 'x null)]
    [(hasheq 'x '(1 2 3))])

   (check-roundtrip
    #<<MOD
syntax = 'proto3';

message Test {
  string s = 1;
  map<int32, Test> m = 2;
}
MOD
    [(hasheq)
     (hasheq 's "")]
    [(hasheq 's "hello")
     (hasheq 's "hello")]
    [(hasheq 's "hello" 'm (hash 1 (hasheq)))
     (hasheq 's "hello" 'm (hash 1 (hasheq 's "")))]
    [(hasheq 's "hello" 'm (hash 1 (hasheq 's "goodbye")))])

   (check-roundtrip
    #<<MOD
syntax = 'proto3';

message Test {
  A a = 1;
}

message A {
  B b = 1;
}

message B {
  int32 x = 1;
}
MOD
    [(hasheq 'a (hasheq))
     (hasheq 'a (hasheq 'b #f))]
    [(hasheq 'a (hasheq 'b (hasheq)))
     (hasheq 'a (hasheq 'b (hasheq 'x 0)))]
    [(hasheq 'a (hasheq 'b (hasheq 'x 42)))])

   (check-roundtrip
    #<<MOD
syntax = 'proto2';

message Test {
  message Inner {
    optional int32 x = 1;
    optional string s = 5 [default = "default"];
  }

  repeated Inner i = 1;
}
MOD
    [(hasheq)
     (hasheq 'i null)]
    [(hasheq 'i (list (hasheq 'x 1 's "hello")))]
    [(hasheq 'i (list (hasheq 'x 1)))
     (hasheq 'i (list (hasheq 'x 1 's "default")))])

   (check-roundtrip
    #<<MOD
syntax = 'proto3';

message Test {
  message Inner {
    int32 x = 1;
    string s = 5;
  }

  repeated Inner i = 1;
}
MOD
    [(hasheq)
     (hasheq 'i null)]
    [(hasheq 'i (list (hasheq 'x 1 's "hello")))]
    [(hasheq 'i (list (hasheq 'x 1)))
     (hasheq 'i (list (hasheq 'x 1 's "")))])

   (check-roundtrip
    #<<MOD
syntax = 'proto2';

message Test {
  optional Test t = 1;
}
MOD
    [(hasheq)
     (hasheq 't #f)]
    [(hasheq 't (hasheq))
     (hasheq 't (hasheq 't #f))]
    [(hasheq 't (hasheq 't #f))])

   (check-roundtrip
    #<<MOD
syntax = 'proto2';

message Test {
  optional bytes b = 1 [default = 'hello'];
}
MOD
    [(hasheq)
     (hasheq 'b #"hello")]
    [(hasheq 'b #"goodbye")])

   (check-roundtrip
    #<<MOD
syntax = 'proto2';

message Test {
  oneof o {
    string s = 1;
    int32 i = 2;
  }
}
MOD
    [(hasheq)
     (hasheq 's "" 'i 0)]
    [(hasheq 's "hello")
     (hasheq 's "hello" 'i 0)]
    [(hasheq 'i 42)
     (hasheq 's "" 'i 42)])

   (check-roundtrip
    #<<MOD
syntax = "proto3";
package schema_reg_protobuf;

option java_package = "io.defn.schema_reg_protobuf";
option java_outer_classname = "PaymentProtos";
option java_multiple_files = true;

message Payment {
  string id = 1;
  double amount = 2;
  string region = 3;
  repeated .schema_reg_protobuf.Payment.MetadataEntry metadata = 4;

  message MetadataEntry {
    option map_entry = true;

    string key = 1;
    string value = 2;
  }
}

message Test {
  Payment p = 1;
}
MOD
    [(hasheq)
     (hasheq 'p #f)]
    [(hasheq 'p (hasheq 'id "1" 'amount 1024.0 'region "USA" 'metadata (list (hasheq 'key "k" 'value "v"))))])

   (check-roundtrip
    #<<MOD
syntax = "proto3";
package schema_reg_protobuf;

option java_package = "io.defn.schema_reg_protobuf";
option java_outer_classname = "PaymentProtos";
option java_multiple_files = true;

message Payment {
  string id = 1;
  double amount = 2;
  string region = 3;
  repeated Qual.Entry metadata = 4;

  message Qual {
    message Entry {
      string key = 1;
      string value = 2;
    }

    string ignored = 1;
  }
}

message Test {
  Payment p = 1;
}
MOD
    [(hasheq)
     (hasheq 'p #f)]
    [(hasheq 'p (hasheq 'id "1" 'amount 1024.0 'region "USA" 'metadata (list (hasheq 'key "k" 'value "v"))))])))

(module+ test
  (require rackunit/text-ui)
  (when protoc
    (run-tests oracle-suite)))
