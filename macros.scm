(import (rnrs)
        (utils))

;; The Haskell "do" syntax
(define-syntax do
  (lambda (stx)
    (syntax-case stx (<-)
      [(_ expression)
       (syntax expression)]
      [(_ (x <- px) expression ...)
       (syntax (bind px (lambda (x) 
                          (do expression ...))))])))

;; === base ===

(define parse
  (lambda (parser text)
    (parser (string->list text))))

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

(define choice
  (lambda (parsers)
    (fold-left or-else (car parsers) (cdr parsers))))

;; Applies parser px. If px fails, returns the value y.
(define option
  (lambda (px y)
    (or-else px (return y))))

;; Fails if parser px fails. Otherwise discards result and continues parsing.
(define optional
  (lambda (px)
    (or-else (do (x <- px)
                 (return '()))
             (return '()))))

;; Also named ".>>", parses two values and discards the right.
(define left
  (lambda (px py)
    (map-f (lambda (xy)
             (let ([x (car xy)]
                   [y (cdr xy)])
               x))
           (and-then px py))))

;; Also named ">>.", parses two values and discards the left.
(define right
  (lambda (px py)
    (map-f (lambda (xy)
             (let ([x (car xy)]
                   [y (cdr xy)])
               y))
           (and-then px py))))


;; Parses three values, and, if successful, discards the left and the right values.
(define between
  (lambda (px py pz)
    (left (right px py) pz)))

;; === sequences ===

(define and-then
  (lambda (px py)
    (do (x <- px)
        (y <- py)
        (return (cons x y)))))


(define sequence
  (lambda (parsers)
    (fold-right and-then (return '()) parsers)))

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

;; === parsers ===

(define character
  (lambda (x)
    (satisfy (lambda (y) (char=? x y)))))

(define digit 
  (satisfy char-numeric?))

(define letter
  (satisfy char-alphabetic?))

(define space 
  (satisfy char-whitespace?))
