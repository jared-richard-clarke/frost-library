(import (rnrs))

;; The Haskell "do" syntax
(define-syntax do
  (lambda (stx)
    (syntax-case stx (<-)
      [(_ expr)
       (syntax expr)]
      [(_ (x <- px) expr ...)
       (syntax (bind px (lambda (x) 
                          (do expr ...))))])))

(define empty? null?)

;; === base ===

(define item
  (lambda (x)
    (if (empty? x)
        x
        (list (car x) (cdr x)))))

;; === monad ====

;; Also named "unit".
(define return
  (lambda (x)
    (lambda (input)
      (list x input))))

;; Also named ">>=".
;; Integrates the sequencing of parsers with the processing of their results.
(define bind
  (lambda (px f)
    (lambda (input)
      (let ([x (px input)])
        (if (empty? x)
            x
            ((f (car x)) (cadr x)))))))

;; Also named "empty".
(define zero
  (lambda ()
    (lambda input '())))

;; === functor ===

(define map-f
  (lambda (f px)
    (do (x <- px)
        (return (f x)))))

;; === applicative ===

(define apply-p
  (lambda (pf px)
    (do (f <- pf)
        (map-f f px))))

;; (define apply-p
;;   (lambda (pf px)
;;     (do (f <- pf)
;;         (x <- px)
;;         (return (f x)))))

;; === satisfy ===

(define satisfy
  (lambda (predicate)
    (do (x <- item)
        (if (predicate x)
            (return x)
            (zero)))))

;; === choices ===

(define or-else
  (lambda (px py)
    (lambda (input)
      (let ([x (px input)])
        (if (empty? x)
            (py input)
            x)))))

(define and-then
  (lambda (px py)
    (do (x <- px)
        (y <- py)
        (return (cons x y)))))

(define many
  (lambda (px)
    (or-else (do (x  <- px)
                 (xs <- (many px))
                 (return (cons x xs)))
             (return '()))))

(define many-1
  (lambda (px)
    (do (x  <- px)
        (xs <- (many px))
        (return (cons x xs)))))
