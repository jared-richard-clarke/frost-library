;; === UNDER CONSTRUCTION ===

(import (rnrs)
        (frost combinators)
        (frost parsers)
        (frost parse))

;; === csv parser ===

;; In Scheme, forward references are allowed only within the bodies of function
;; definitions - a.k.a lambda abstractions. Wrapping each parser within an extra
;; lambda abstraction both allows the construction of mutually-defined grammars
;; and simplifies incremental parser construction.

(define comma      (character #\,))
(define quote-mark (character #\"))
(define HEADER 'Header)
(define ROW 'Row)

;; csv ::= header row+ EOF
(define csv
  (lambda (input)
    ((sequence csv-header (many-1 csv-row)) input)))

;; header ::= row
(define csv-header
  (lambda (input)
    ((monad-do (hr <- csv-row)
               (return (cons HEADER hr)))
     input)))

;; row ::= field ("," field)* "\n"
(define csv-row
  (lambda (input)
    ((monad-do (fs <- (sep-by-1 csv-field comma))
               (lf <- linefeed)
               (return (cons ROW fs)))
     input)))

;; field ::= text | string
(define csv-field
  (lambda (input)
    ((choice csv-text csv-string) input)))

;; text ::= ![,\n"]+
(define csv-text
  (lambda (input)
    ((fmap list->string (many-1 (none-of ",\n\""))) input)))

;; string ::= '"' !('"')* '"'
(define csv-string
  (lambda (input)
    ((between quote-mark
              (fmap list->string (many (none-of "\"")))
              quote-mark)
     input)))
