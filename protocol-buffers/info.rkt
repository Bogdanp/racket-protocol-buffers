#lang info

(define collection "protocol-buffers")
(define deps '("base"
               "protocol-buffers-lib"))
(define build-deps '("racket-doc"
                     "scribble-lib"))
(define implies '("protocol-buffers-lib"))
(define scribblings '(("protocol-buffers-manual.scrbl" () (parsing-library))))
