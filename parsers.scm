(import (rnrs))

;; === base ===

(define (item lst)
  (if (null? lst)
      (list)
      (list (car lst) (cdr lst))))

;; === monad ===

(define (zero) (lambda xs (list)))

(define (return x)
  (lambda (input)
    (list x input)))

(define (bind p f)
  (lambda (input)
    (let ([lst (p input)])
      (if (null? lst)
          (list)
          ((f (car lst)) (cadr lst))))))

;; === functor ===

(define (map-f f p)
  (bind p (lambda (x) (return (f x)))))

;; === applicative ===

(define (apply-p pf px)
  (bind pf (lambda (f)
             (bind px (lambda (x)
                        (return (f x)))))))

;; === alternatives ===

(define (or-else p q)
  (lambda (input)
    (let ([lst (p input)])
      (if (null? lst)
          (q input)
          lst))))

(define (and-then p q)
  (bind p (lambda (pv)
            (bind q (lambda (qv)
                      (return (list pv qv)))))))

;; === derived primitives ===

(define (satisfy predicate)
  (bind item (lambda (x)
               (if (predicate x)
                   (return x)
                   (zero)))))

(define (parse-char x)
  (satisfy (lambda (y) (char=? x y))))

