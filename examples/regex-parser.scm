;; === UNDER CONSTRUCTION ===

(import (rnrs)
        (frost combinators)
        (frost parsers)
        (frost parse))

;; === Regular Expression Parser ===

;; In Scheme, forward references are allowed only within the bodies of function
;; definitions - a.k.a lambda abstractions. Wrapping each parser within an extra
;; lambda abstraction both allows the construction of mutually-defined grammars
;; and simplifies incremental parser construction.

(define build-binary
  (lambda (op)
    (lambda (x y)
      (make-binary op x y))))

(define alternate   (replace (character #\|) (return (build-binary "|"))))
(define kleene-star (character #\*))
(define plus        (character #\+))
(define option      (character #\?))
(define concat      (replace empty (return (build-binary "•"))))

(define-record-type binary
  (fields operator left right))

(define-record-type suffix
  (fields operator value))

;; === parsers ===

(define regex
  (lambda (input)
    (expression input)))

(define expression
  (lambda (input)
    ((chain-left-1 alternation alternate) input)))

(define alternation
  (lambda (input)
    ((chain-left-1 concatenation concat) input)))

(define concatenation
  (lambda (input)
    ((monad-do (x <- primary)
               (s <- (optional (one-of "*+?")))
               (if (null? s)
                   (return x)
                   (return (make-suffix s x))))
     input)))

(define primary
  (lambda (input)
    ((choice (between (character #\()
                      expression
                      (character #\)))
             (none-of "*+?|"))
     input)))
