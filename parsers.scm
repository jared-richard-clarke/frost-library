(import (rnrs))

;; === base ===

(define item
  (lambda (lst)
    (if (null? lst)
        (list)
        (list (car lst) (cdr lst)))))

;; === monad ===

(define zero
  (lambda ()
    (lambda xs (list))))

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

(define map-f
  (lambda (f p)
    (bind p (lambda (x) (return (f x))))))

;; === applicative ===

(define apply-p
  (lambda (pf px)
    (bind pf (lambda (f) (map-f f px)))))

;; apply-p defined only by bind
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
