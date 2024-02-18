;; === UNDER CONSTRUCTION ===

(import (rnrs)
        (frost combinators)
        (frost parsers)
        (frost parse))

;; === Arithmetic Parser and Evaluator ===

;; In Scheme, forward references are allowed only within the bodies of function
;; definitions - a.k.a lambda abstractions. Wrapping each parser within an extra
;; lambda abstraction both allows the construction of mutually-defined grammars
;; and simplifies incremental parser construction.

(define add (replace (character #\+) (return +)))
(define sub (replace (character #\-) (return -)))
(define mul (replace (character #\*) (return *)))
(define div (replace (character #\/) (return /)))
(define pow (replace (character #\^) (return expt)))

(define add-sub (choice add sub))
(define mul-div (choice mul div))

;; Expression <- Term ( [+-] Term )*
(define expression
  (lambda (input)
    ((chain-left-1 term add-sub) input)))

;; Term <- Factor ( [*/] Factor )*
(define term
  (lambda (input)
    ((chain-left-1 factor mul-div) input)))

;; Factor <- Base ( "^" Base )*
(define factor
  (lambda (input)
    ((chain-right-1 base-expr pow) input)))

;; Base   <- "(" Expression ")" / Number
;; Number <- ( "+" / "-" )? ( "0" / [1-9] [0-9]* ( "." [0-9]+ )? )
(define base-expr
  (lambda (input)
    ((choice (between (character #\()
                      expression
                      (character #\)))
             real)
     input)))
