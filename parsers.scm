(import (rnrs))

;; === UNDER HEAVY CONSTRUCTION ===

;; Unlike Haskell or OCaml, which are statically typed, this Scheme implementation
;; doesn't wrap its functions in a Parser type. Functions return parsing functions.
;;
;; Parser functions input a list of chars and output either an empty list on failure
;; or a list containing a result and the remaining list of chars.
;;
;; (parse-s (string->list "sam")) -> '(#\s (#\a #\m))

;; === base ===

(define item
  (lambda (lst)
    (if (null? lst)
        '()
        (list (car lst) (cdr lst)))))

;; === monad zero ===

(define zero
  (lambda ()
    (lambda input '())))

;; === monad ====

(define return
  (lambda (x)
    (lambda (input)
      (list x input))))

(define bind
  (lambda (p f)
    (lambda (input)
      (let ([lst (p input)])
        (if (null? lst)
            '()
            ((f (car lst)) (cadr lst)))))))

;; === functor ===

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

(define lift-2
  (lambda (f)
    (lambda (px py)
      (apply-p (apply-p (return f) px) py))))

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
  (lambda parsers
    (fold-left or-else (car parsers) (cdr parsers))))

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
