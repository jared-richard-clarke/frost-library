(import (rnrs))

;; === UNDER HEAVY CONSTRUCTION ===

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

;; Also known as "unit".
(define return
  (lambda (x)
    (lambda (input)
      (list x input))))

;; Also known as ">>="
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

;; === monad plus ===

(define plus
  (lambda (px py)
    (lambda (input)
      (append (px input) (py input)))))

;; === choices ===

(define choice
  (lambda (px py)
    (lambda (input)
      (let ([x ((plus px py) input)])
        (if (null? x)
            '()
            (list (car x)))))))

(define and-then
  (lambda (px py)
    (bind px (lambda (x)
               (bind py (lambda (y)
                          (return (cons x y))))))))

(define optional
  (lambda (px)
    (plus px (return '()))))

(define any-of
  (lambda parsers
    (fold-left choice (car parsers) (cdr parsers))))

;; === sequences ===

(define many
  (lambda (px)
    (optional (many-1 px))))

(define many-1
  (lambda (px)
    (bind px (lambda (x)
               (bind (many px) (lambda (xs)
                                 (return (cons x xs))))))))

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
