#lang scribble/manual

@(require scribble/example
          (for-label protocol-buffers
                     racket/base
                     racket/contract
                     racket/port
                     racket/string))

@title{Protocol Buffers}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]

@(define (proto-link . pre-content)
  (apply link "https://protobuf.dev/" pre-content))

This package implements support for the @proto-link{Protocol Buffers}
serialization format.  It implements a parser for the Protocol Buffers
definition languages @tt{proto2} and @tt{proto3}, and it does not have
any external dependencies.

@examples[
  (require protocol-buffers
           racket/port
           racket/string)
  (define m
    (read-protobuf
      (open-input-string
       (string-join
        '("syntax = 'proto2';"
          ""
          "message Person {"
          "  required string name = 1;"
          "  required uint32 age = 2;"
          "  optional string favorite_food = 5;"
          "}")
        "\n"))))
  (define Person (mod-ref m 'Person))
  (define data (hasheq 'name "Bogdan" 'age 30))
  (define person-bs
   (call-with-output-bytes
    (lambda (out)
      (write-message Person data out))))
  (println person-bs)
  (call-with-input-bytes person-bs
   (lambda (in)
     (read-message Person in)))
]

@section{Limitations}

The parser supports RPC and stream definitions, but @tech{modules} do
not.  Likewise, @tech{message} group fields are supported in the
parser, but definitions that use them raise an error when read via
@racket[read-protobuf].

@section{Reference}
@defmodule[protocol-buffers]

@subsection{Modules}

A @deftech{module} is a runtime representation of a Protocol Buffers
definition and any of its dependencies.

@defproc[(read-protobuf [in input-port? (current-input-port)]) mod?]{
  Reads a protobuf definition into a @tech{module} from @racket[in].

  If the module contains any @tt{import} statements, then the imported
  files will also be read relative to @racket[current-directory].
}

@defproc[(mod? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a @tech{module}.
}

@defproc[(mod-package [m mod?]) (or/c #f symbol?)]{
  Returns @racket[m]'s package, if any.
}

@defproc[(mod-options [m mod?]) hash?]{
  Returns the top-level options defined in @racket[m].
}

@defproc[(mod-ref [m mod?]
                  [id symbol?]
                  [default-proc (-> any) _error]) message?]{

  Returns the @tech{message} named @racket[id] from @racket[m].  If
  @racket[m] does not contain a message with that name, then the
  result of applying @racket[default-proc] is returned instead.
}

@subsection{Messages}

@deftech{Message} correspond to @tt{message} definitions within a
Protocol Buffers file.  They contain information about how data should
be serialized and deserialized.

@defproc[(message? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a @tech{message}.
}

@defproc[(message-name [m message?]) symbol?]{
  Returns the name of the message @racket[m].
}

@defproc[(message-options [m message?]) hash?]{
  Returns any options defined on the message @racket[m].
}

@defproc[(read-message [m message?]
                       [in input-port? (current-input-port)]) hash?]{
  Reads data from @racket[in] according to the format of @racket[m].
}

@defproc[(write-message [m message?]
                        [v hash?]
                        [out output-port? (current-output-port)]) void?]{

  Writes @racket[v] to @racket[out] according to the format of @racket[m].
}
