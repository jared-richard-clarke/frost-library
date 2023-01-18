(import (rnrs))

;; === UNDER HEAVY CONSTRUCTION ===

;; A parser combinator library is a functional approach to building recursive descent parsers.
;; Basic functions implement parsers, whereas higher-order functions (combinators) implement
;; grammar constructions such as choice, sequencing, and repetition.
;;
;; Unlike Haskell or OCaml, which are statically typed, this Scheme implementation
;; doesn't wrap its functions in a "Parser" type. Functions return parser functions.
;;
;; Parser functions input a list of chars and output either an empty list when they fail
;; or a list containing a value and the remaining list of chars when they succeed.
;;
;; success: (function (list char)) -> (list value (list char))
;; failure: (function (list char)) -> (list)
;;
;; (parse-s (string->list "sam")) -----> '(#\s (#\a #\m))
;; (parse-digit (string->list "sam")) -> '()

;; === base ===

(define item
  (lambda (x)
    (if (null? x)
        '()
        (list (car x) (cdr x)))))

;; === monad ====

;; Also known as "unit" or "result".
(define return
  (lambda (x)
    (lambda (input)
      (list x input))))

;; Integrates the sequencing of parsers with the processing of their results.
(define bind
  (lambda (px f)
    (lambda (input)
      (let ([x (px input)])
        (if (null? x)
            '()
            ((f (car x)) (cadr x)))))))

;; === monad zero ===

(define zero
  (lambda ()
    (lambda input '())))

;; === choices ===

(define or-else
  (lambda (px py)
    (lambda (input)
      (let ([x (px input)])
        (if (null? x)
            (py input)
            x)))))

(define optional
  (lambda (px)
    (or-else px (return '()))))

(define any-of
  (lambda parsers
    (fold-left or-else (car parsers) (cdr parsers))))

;; === sequences ===

(define and-then
  (lambda (px py)
    (bind px (lambda (x)
               (bind py (lambda (y)
                          (return (cons x y))))))))

;; === derived primitives ===

(define satisfy
  (lambda (predicate)
    (bind item (lambda (x)
                 (if (predicate x)
                     (return x)
                     (zero))))))

(define parse-char
  (lambda (x)
    (satisfy (lambda (y) (char=? x y)))))

(define parse-digit 
  (satisfy char-numeric?))

(define parse-letter
  (satisfy char-alphabetic?))

(define parse-space 
  (satisfy char-whitespace?))

(define parse-string
  (or-else (bind parse-letter (lambda (x)
                                (bind parse-string (lambda (xs)
                                                     (return (cons x xs))))))
           (return '())))
