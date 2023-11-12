(library (frost parse)
         (export parse)
         (import (rnrs base)
                 (rnrs io simple)
                 (frost combinators)
                 (frost data)
                 (frost parsers))
         
         ;; Applies a parser to a string and outputs a tagged result. Lifts string into monadic context
         ;; before applying parser.
         ;; (parse (text "abc") "abc") -> result:struct(OK, "abc")
         ;; (parse (text "abc") "xyz") -> result:struct(ERROR, "line: 1, column: 1. Expected: abc, Got: x")
         ;; Side Note: prints "reply", "state", "want", and "output" to the standard output for debugging.
         ;; This is temporary.
         (define parse
           (lambda (parser text)
             (let-values ([(reply state want output)
                           (parser (let* ([input  (string->vector text)]
                                          [length (vector-length input)]
                                          [offset 0]
                                          [line   1]
                                          [column 1])
                                     (make-state input length offset line column)))])
               (begin (write reply)
                      (newline)
                      (write state)
                      (newline)
                      (write want)
                      (newline)
                      (write output)
                      (newline)
                      (if (eq? (cdr reply) ERROR)
                          (make-result ERROR (format-error state want))
                          (make-result OK    output))))))
         
         )
