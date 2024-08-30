(library (frost matcher)
         (export match)
         (import (rnrs base)
                 (frost combinators)
                 (frost data)
                 (frost parsers))

         (define INPUT-ERROR "Input must be of type string.")
         
         ;; Applies a parser to a string and outputs a tagged result.
         ;; Lifts string into monadic context before applying parser.
         ;; (match (text "abc") "abc") -> struct:result{OK, "abc"}
         ;; (match (text "abc") "xyz") -> struct:result{ERROR, "line: 1, column: 1. Expected: abc, Got: x"}
         (define match
           (lambda (parser text)
             (if (not (string? text))
                 (make-result ERROR INPUT-ERROR)
                 (let-values ([(reply state want output)
                               (parser (let* ([input  (string->vector text)]
                                              [length (vector-length input)]
                                              [offset 0]
                                              [line   1]
                                              [column 1])
                                         (make-state input length offset line column)))])
                   (if (eq? (cdr reply) ERROR)
                       (make-result ERROR (format-error state want))
                       (make-result OK    output))))))
         
         )
