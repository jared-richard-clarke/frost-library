;; === UNDER CONSTRUCTION ===

(import (rnrs)
        (frost combinators)
        (frost parsers)
        (frost parse))

;; === csv parser ===

(define comma      (character #\,))
(define quote-mark (character #\"))
(define header-label 'header)
(define body-label  'body)
(define row-label    'row)

;; csv ::= header row+ EOF
(define csv
  (lambda (state)
    ((sequence csv-header (many-1 csv-row)) state)))

;; header ::= row
(define csv-header
  (lambda (state)
    ((monad-do (hr <- csv-row)
               (return (cons header-label hr)))
     state)))

;; row ::= field ("," field)* "\n"
(define csv-row
  (lambda (state)
    ((monad-do (fs <- (sep-by-1 csv-field comma))
               (lf <- linefeed)
               (return (cons row-label fs)))
     state)))

;; field ::= text | string
(define csv-field
  (lambda (state)
    ((choice csv-text csv-string) state)))

;; text ::= ![,\n"]+
(define csv-text
  (lambda (state)
    ((fmap list->string (many-1 (none-of ",\n\""))) state)))

;; string ::= '"' !('"')* '"'
(define csv-string
  (lambda (state)
    ((between quote-mark
              (fmap list->string (many (none-of "\"")))
              quote-mark)
     state)))
