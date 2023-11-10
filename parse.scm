(library (frost parse)
         (export parse-string)
         (import (rnrs base)
                 (rnrs io simple)
                 (frost combinators)
                 (frost data)
                 (frost parsers))
         
         ;; Applies a parser to a string and outputs an arbitrary value dictated by the parser.
         ;; Side Note: prints "reply", "state", "want", and "output" to the standard output for debugging.
         ;; This is temporary.
         (define parse-string
           (lambda (parser text)
             (let-values ([(reply state want output)
                           (parser (make-state (string->list text) 1 0 state-update-char))])
               (begin (write reply)
                      (newline)
                      (write state)
                      (newline)
                      (write want)
                      (newline)
                      (write output)
                      output))))
         
         )
