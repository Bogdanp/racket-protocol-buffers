#lang racket/base

(require rackunit
         "common.rkt")

(define proto3-parser-suite
  (test-suite
   "proto3-parser"

   (test-case "parse example .proto file from spec"
     (check-equal?
      (parse-example "proto3-spec-example.proto")
      '(Proto3
        ((Import public "other.proto")
         (Option (java_package) "com.example.foo")
         (Enum
          EnumAllowingAlias
          ((EnumField EAA_UNSPECIFIED 0 ())
           (EnumField EAA_STARTED 1 ())
           (EnumField EAA_RUNNING 1 ())
           (EnumField EAA_FINISHED 2 ((Option (custom_option) "hello world"))))
          ((Option (allow_alias) #t))
          ())
         (Message
          Outer
          ((Message
            Inner
            ()
            ((MessageField optional int64 ival 1 ()))
            ()
            ()
            ()))
          ((MessageField repeated Inner inner_message 2 ())
           (MessageField optional EnumAllowingAlias enum_field 3 ())
           (MessageMapField int32 string my_map 4 ()))
          ((Option (my_option a) #t))
          ()
          ())))))

   (test-case "parse example service proto from spec"
     (check-equal?
      (parse-example "proto3-service.proto")
      '(Proto3
        ((Import #f "search.proto")
         (Service
          SearchService
          ((RPC Search #f SearchRequest #f SearchResponse ()))
          ()
          ())))))

   (test-case "parse .proto file with repeated scalar"
     (check-equal?
      (parse-example "proto3-packed-repeated.proto")
      '(Proto3
        ((Message
          Foo
          ((Message Inner () ((MessageField optional string s 1 ())) () () ()))
          ((MessageField repeated int64 x 1 ((Option (packed) #t)))
           (MessageField optional int64 y 2 ())
           (MessageField repeated Inner i 3 ()))
          ()
          ()
          ())))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests proto3-parser-suite))
