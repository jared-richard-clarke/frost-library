(import (rnrs)
        (utils))

;; === UNDER HEAVY CONSTRUCTION ===

;; Applicative parsers are functions that process characters.
;; Parser combinators are higher-order functions, composed of applicative parsers,
;; that process grammar constructions — choice, sequencing, repetition, etc.
;;
;; (parser (list char)) -> (list) | (list any (list char))

;; The Haskell "do" syntax (simplified). Makes monads readable.
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

;; Also named "unit". Also called "pure" within the 
;; context of Applicative functors.

(define return
  (lambda (x)
    (lambda (input)
      (list x input))))

;; Also named ">>=".
;; In this context, integrates the sequencing of parsers 
;; with the processing of their results.
(define bind
  (lambda (px f)
    (lambda (input)
      (let ([x (px input)])
        (if (empty? x)
            x
            ((f (car x)) (cadr x)))))))

;; Also named "empty".
(define zero (lambda xs '()))

;; === functor ===

(define map-f
  (lambda (f px)
    (do (x <- px)
        (return (f x)))))

;; (define map-f
;;   (lambda (f px)
;;     (bind px (lambda (x)
;;                (return (f x))))))

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

;; Side Note: sequences of applicatives are used to lift multi-parameter
;; functions into a monadic context. However, this process works only
;; with curried functions because each argument is applied in its own monad.
;; In a language like Scheme, it's simpler and more efficient to chain a series
;; of monads together and then call the combining function at the end.

;; === satisfy ===

(define satisfy
  (lambda (predicate)
    (do (x <- item)
        (if (predicate x)
            (return x)
            zero))))


;; (define satisfy
;;   (lambda (predicate)
;;     (bind item (lambda (x)
;;                  (if (predicate x)
;;                      (return x)
;;                      fail)))))

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
             zero)))

;; Also named ".>>", parses two values and discards the right.
(define left
  (lambda (px py)
    (do (x <- px)
        (y <- py)
        (return x))))

;; Also named ">>.", parses two values and discards the left.
(define right
  (lambda (px py)
    (do (x <- px)
        (y <- py)
        (return y))))

;; Parses three values, and, if successful, discards the left and the right values.
(define between
  (lambda (px py pz)
    (do (x <- px)
        (y <- py)
        (z <- pz)
        (return y))))

;; === sequences ===

(define and-then
  (lambda (px py)
    (do (x <- px)
        (y <- py)
        (return (cons x y)))))

;; (define and-then
;;   (lambda (px py)
;;     (bind px (lambda (x)
;;                (bind py (lambda (y)
;;                           (return (cons x y))))))))

(define sequence
  (lambda (parsers)
    (fold-right and-then (return '()) parsers)))

(define many
  (lambda (px)
    (or-else (many-1 px)
             (return '()))))

(define many-1
  (lambda (px)
    (do (x  <- px)
        (xs <- (many px))
        (return (cons x xs)))))

;; (define many-1
;;   (lambda (px)
;;     (bind px (lambda (x)
;;                (bind (many px) (lambda (xs)
;;                                 (return (cons x xs))))))))

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

(define digits (many-1 digit))

(define letters (many-1 letter))

(define spaces (many space))
