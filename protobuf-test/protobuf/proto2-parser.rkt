#lang racket/base

(require rackunit
         "common.rkt")

(define proto2-parser-suite
  (test-suite
   "proto2-parser"

   (test-case "parse example .proto file from spec"
     (check-equal?
      (parse-example "proto2-spec-example.proto")
      '((Import public "other.proto")
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
           ((MessageField required int64 ival 1 ()))
           ()
           ()
           ()))
         ((MessageField repeated Inner inner_message 2 ())
          (MessageField optional EnumAllowingAlias enum_field 3 ())
          (MessageMapField int32 string my_map 4 ()))
         ((Option (my_option a) #t))
         ()
         (((Range 20 30))))
        (Message
         Foo
         ()
         ((MessageGroupField optional GroupMessage 1 ()
                             ((MessageField optional bool a 1 ()))
                             ()
                             ()
                             ()))
         ()
         ()
         ()))))

   (test-case "parse example service proto from spec"
     (check-equal?
      (parse-example "proto2-service.proto")
      '((Import #f "search.proto")
        (Service
         SearchService
         ((RPC Search #f SearchRequest #f SearchResponse ()))
         ()
         ()))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests proto2-parser-suite))
