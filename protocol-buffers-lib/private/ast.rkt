#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse/pre))

(provide
 (struct-out Node))

(struct Node (loc) #:transparent)

(define-syntax (define-node-types stx)
  (syntax-parse stx
    [(_ [Id:id (fld:id ...)] ...)
     #:with (Id? ...)
     (for/list ([stx (in-list (syntax-e #'(Id ...)))])
       (format-id stx "~a?" stx))
     #:with ((Id-fld ...) ...)
     (for/list ([id-stx (in-list (syntax-e #'(Id ...)))]
                [flds-stx (in-list (syntax-e #'((fld ...) ...)))])
       (for/list ([fld-stx (in-list (syntax-e flds-stx))])
         (format-id fld-stx "~a-~a" id-stx fld-stx)))
     #'(begin
         (struct Id Node (fld ...) #:transparent) ...
         (provide ->sexp (struct-out Id) ...)
         (define (->sexp n)
           (cond
             [(Id? n) `(Id ,(->sexp (Id-fld n)) ...)] ...
             [(list? n) (map ->sexp n)]
             [else n])))]))

(define-node-types
  [Proto2 (children)]
  [Proto3 (children)]
  [Import (qualifier path)]
  [Package (name)]
  [Option (path value)]
  [EnumField (name value options)]
  [Enum (name fields options reserved)]
  [Range (lo hi)]
  [MessageField (label type name number options)]
  [MessageGroupField (label name number children fields options reserved extensions)]
  [MessageOneOfField (name fields)]
  [MessageMapField (key-type val-type name number options)]
  [Message (name children fields options reserved extensions)]
  [Extend (name fields)]
  [RPC (name stream-domain? domain stream-range range? options)]
  [Stream (name domain range options)]
  [Service (name rpcs streams options)])
