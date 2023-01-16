(import (rnrs))

;; === UNDER HEAVY CONSTRUCTION ===

;; Unlike Haskell or OCaml, which are statically typed, this Scheme implementation
;; doesn't wrap its functions in a "Parser" type. Functions return parser functions.
;;
;; Parser functions input a list of chars and output either an empty list when it fails
;; or a list containing a value and the remaining list of chars when it succeeds.
;;
;; success: (function (list char)) -> (list value (list char))
;; failure: (function (list char)) -> (list)
;;
;; (parse-s (string->list "sam")) -----> '(#\s (#\a #\m))
;;
;; (parse-digit (string->list "sam")) -> '()

;; === base ===

(define item
  (lambda (lst)
    (if (null? lst)
        (list)
        (list (car lst) (cdr lst)))))

;; === monad zero ===

(define zero
  (lambda ()
    (lambda input (list))))

;; === monad ====

;; Also known as "unit".
(define return
  (lambda (x)
    (lambda (input)
      (list x input))))

(define bind
  (lambda (p f)
    (lambda (input)
      (let ([lst (p input)])
        (if (null? lst)
            (list)
            ((f (car lst)) (cadr lst)))))))

;; === functor ===

;; Also known as "lift" or "lift-1"
(define map-f
  (lambda (f p)
    (bind p (lambda (x) (return (f x))))))

;; === applicative ===

(define apply-p
  (lambda (pf px)
    (bind pf (lambda (f) (map-f f px)))))

;; apply-p defined solely by bind
;;
;; (define apply-p
;;   (lambda (pf px)
;;     (bind pf (lambda (f)
;;                (bind px (lambda (x)
;;                          (return (f x))))))))

;; === alternatives ===

(define or-else
  (lambda (p q)
    (lambda (input)
      (let ([lst (p input)])
        (if (null? lst)
            (q input)
            lst)))))

(define and-then
  (lambda (p q)
    (bind p (lambda (pv)
              (bind q (lambda (qv)
                        (return (list pv qv))))))))

(define choice
  (lambda (parsers)
    (fold-left or-else (car parsers) (cdr parsers))))

(define any-of
  (lambda (chars)
    (choice (map parse-char chars))))

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

(define parse-alpha 
  (satisfy char-alphabetic?))

(define parse-space 
  (satisfy char-whitespace?))
