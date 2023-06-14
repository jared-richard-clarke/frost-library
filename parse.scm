(library (frost parse)
         (export parse-string)
         (import (rnrs base)
                 (rnrs io simple)
                 (frost combinators)
                 (frost parsers))
         
         ;; Applies a parser to a string and outputs an arbitrary value dictated by the parser.
         ;; Side Note: prints "reply", "state", and "output" to the standard output for debugging.
         ;; This is temporary.
         (define parse-string
           (lambda (parser text)
             (let-values ([(reply state output)
                           (parser (make-state (string->list text) 1 0))])
               (begin (write reply)
                      (newline)
                      (write state)
                      (newline)
                      (write output)
                      output))))
         
         )
