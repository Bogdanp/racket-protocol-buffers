#lang racket/base

(require racket/fixnum
         racket/match
         racket/port
         "module.rkt"
         "varint.rkt")

(provide
 write-message)

(define (write-message m h [out (current-output-port)])
  (match-define (message _name _options fields) m)
  (define names-to-numbers
    (for/hasheq ([(n f) (in-hash fields)])
      (values (message-field-name f) n)))
  (for ([(k v) (in-hash h)])
    (define num (hash-ref names-to-numbers k #f))
    (unless num
      (error 'write-message "unknown field: ~a" k))
    (match-define (message-field _label type name _options)
      (hash-ref fields num))
    (write-field* num name type v out)))

(define (write-field* num name type value out)
  (if (list? value)
      (for ([v (in-list value)])
        (write-field* num name type v out))
      (write-field num name type value out)))

(define (write-field num name type value out)
  (match type
    ;; 0
    ['bool
     (write-tl num 0 out)
     (write-uvarint (if value 1 0) out)]
    [(or 'int32 'int64)
     (unless (exact-integer? value)
       (error 'write-field "expected an exact integer~n  got: ~e~n  field: ~a" value name))
     (write-tl num 0 out)
     (cond
       [(< value 0)
        (define mask
          (if (eq? type 'int32)
              #xFFFFFFFF
              #xFFFFFFFFFFFFFFFF))
        (write-uvarint (bitwise-and mask (+ mask value 1)) out)]
       [else
        (write-uvarint value out)])]
    [(or 'uint32 'uint64)
     (unless (exact-nonnegative-integer? value)
       (error 'write-field "expected a nonnegative integer~n  got: ~e~n  field: ~a" value name))
     (write-tl num 0 out)
     (write-uvarint value out)]
    [(or 'sint32 'sint64)
     (unless (exact-integer? value)
       (error 'write-field "expected an exact integer~n  got: ~e~n  field: ~a" value name))
     (write-tl num 0 out)
     (write-varint value out)]
    [(enum enum-name _options fields)
     (unless (hash-has-key? fields value)
       (error 'write-field "enum ~a does not have a case named ~e~n  field: ~a" enum-name value name))
     (write-tl num 0 out)
     (write-uvarint (hash-ref fields value) out)]

    ;; 1
    [(or 'fixed64 'sfixed64)
     (unless (exact-integer? value)
       (error 'write-field "expected an exact integer~n  got: ~e~n  field: ~a" value name))
     (write-tl num 1 out)
     (write-bytes (integer->integer-bytes value 8 (eq? type 'sfixed64) #f) out)]
    ['double
     (unless (real? value)
       (error 'write-field "expected a real~n  got: ~e~n  field: ~a" value name))
     (write-tl num 1 out)
     (write-bytes (real->floating-point-bytes value 8 #f) out)]

    ;; 2
    ['string
     (unless (string? value)
       (error 'write-field "expected a string~n  got: ~e~n  field: ~a" value name))
     (define bs (string->bytes/utf-8 value))
     (write-tl num 2 out)
     (write-uvarint (bytes-length bs) out)
     (write-bytes bs out)]
    ['bytes
     (unless (bytes? value)
       (error 'write-field "expected bytes~n  got: ~e~n  field: ~a" value name))
     (write-tl num 2 out)
     (write-uvarint (bytes-length value) out)
     (write-bytes value out)]
    [(message _name _options _fields)
     (unless (hash? value)
       (error 'write-field "expected a hash~n  got: ~e~n  field: ~a" value name))
     (define bs
       (call-with-output-bytes
        (lambda (bs-out)
          (write-message type value bs-out))))
     (write-tl num 2 out)
     (write-uvarint (bytes-length bs) out)
     (write-bytes bs out)]
    [(map-type k-type v-type)
     (unless (hash? value)
       (error 'write-field "expected a hash~n  got: ~e~n  field: ~a" value name))
     (define bs
       (call-with-output-bytes
        (lambda (bs-out)
          (for ([(k v) (in-hash value)])
            (write-field* 1 "map key" k-type k bs-out)
            (write-field* 2 "map value" v-type v bs-out)))))
     (write-tl num 2 out)
     (write-uvarint (bytes-length bs) out)
     (write-bytes bs out)]

    ;; 3: FIXME
    ;; 4: FIXME

    ;; 5
    [(or 'fixed32 'sfixed32)
     (unless (exact-integer? value)
       (error 'write-field "expected an exact integer~n  got: ~e~n  field: ~a" value name))
     (write-tl num 5 out)
     (write-bytes (integer->integer-bytes value 4 (eq? type 'sfixed32) #f) out)]
    ['float
     (unless (real? value)
       (error 'write-field "expected a real~n  got: ~e~n  field: ~a" value name))
     (write-tl num 5 out)
     (write-bytes (real->floating-point-bytes value 4 #f) out)]))

(define (write-tl num tag out)
  (write-uvarint (fxior (fxlshift num 3) tag) out))
