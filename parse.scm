(library (frost parse)
         (export parse)
         (import (rnrs base)
                 (rnrs io simple)
                 (frost combinators)
                 (frost data)
                 (frost parsers))
         
         ;; Applies a parser to a string and outputs a tagged result.
         ;; Lifts string into monadic context before applying parser.
         ;; (parse (text "abc") "abc") -> struct:result{OK, "abc"}
         ;; (parse (text "abc") "xyz") -> struct:result{ERROR, "line: 1, column: 1. Expected: abc, Got: x"}
         (define parse
           (lambda (parser text)
             (let-values ([(reply state want output)
                           (parser (let* ([input  (string->vector text)]
                                          [length (vector-length input)]
                                          [offset 0]
                                          [line   1]
                                          [column 1])
                                     (make-state input length offset line column)))])
               (if (eq? (cdr reply) ERROR)
                   (make-result ERROR (format-error state want))
                   (make-result OK    output)))))
         
         )
