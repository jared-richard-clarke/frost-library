(import (rnrs)
        (utils))

;; (parser (list char)) -> (list) | (list any (list char))

;; The Haskell "do" syntax (simplified). Makes monads readable.
;; Rename "monad-do" because "do" is probably one of the least
;; descriptive words in the English language.
(define-syntax monad-do
  (lambda (stx)
    (syntax-case stx (<-)
      [(_ expression)
       (syntax expression)]
      [(_ (x <- mx) expression ...)
       (syntax (bind mx (lambda (x) 
                          (monad-do expression ...))))])))

;; === base ===

(define parse
  (lambda (parser text)
    (parser (string->list text))))

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

;; Also named "empty"
(define zero (lambda input '()))

;; === functor ===

(define map-f
  (lambda (f px)
    (monad-do (x <- px)
              (return (f x)))))

;; === applicative ===

(define apply-p
  (lambda (pf px)
    (monad-do (f <- pf)
              (map-f f px))))

;; (define apply-p
;;   (lambda (pf px)
;;     (monad-do (f <- pf)
;;               (x <- px)
;;               (return (f x)))))

;; Side Note: sequences of applicatives are used to lift multi-parameter
;; functions into a monadic context. However, this process works only
;; with curried functions because each argument is applied in its own monad.
;; In a language like Scheme, it's simpler and more efficient to chain a series
;; of monads together and then call the combining function at the end rather
;; than lift a function into a monadic context piecemeal.

;; === satisfy ===

(define satisfy
  (lambda (test)
    (lambda (input)
      (if (empty? input)
          '()
          (let ([x  (car input)]
                [xs (cdr input)])
            (if (not (test x))
                '()
                (list x xs)))))))

;; === choices ===

(define or-else
  (lambda (px py)
    (lambda (input)
      (let ([x (px input)])
        (if (not (empty? x))
            x
            (py input))))))

(define choice
  (lambda (parsers)
    (fold-right or-else zero parsers)))

;; (define choice
;;   (lambda (parsers)
;;    (fold-left or-else (car parsers) (cdr parsers))))

;; Applies parser px. If px fails, returns the value y.
(define option
  (lambda (px y)
    (or-else px (return y))))

;; Applies parser px. If px succeeds, ignore its result and return y.
(define ignore
  (lambda (px y)
    (monad-do (x <- px)
              (return y))))

;; Also named ".>>", parses two values and discards the right.
(define left
  (lambda (px py)
    (monad-do (x <- px)
              (y <- py)
              (return x))))

;; Also named ">>.", parses two values and discards the left.
(define right
  (lambda (px py)
    (monad-do (x <- px)
              (y <- py)
              (return y))))

;; Parses three values, and, if successful, discards the left and the right values.
(define between
  (lambda (px py pz)
    (monad-do (x <- px)
              (y <- py)
              (z <- pz)
              (return y))))

;; === sequences ===

(define and-then
  (lambda (px py)
    (monad-do (x <- px)
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
    (monad-do (x  <- px)
              (xs <- (many px))
              (return (cons x xs)))))

;; (define many-1
;;   (lambda (px)
;;     (bind px (lambda (x)
;;                (bind (many px) (lambda (xs)
;;                                 (return (cons x xs))))))))

(define sep-by
  (lambda (px sep)
    (or-else (sep-by-1 px sep)
             (return '()))))

(define sep-by-1
  (lambda (px sep)
    (monad-do (x  <- px)
              (xs <- (many (monad-do (s <- sep)
                                     (y <- px)
                                     (return y))))
              (return (cons x xs)))))

;; === parsers ===

(define character
  (lambda (x)
    (satisfy (lambda (y) (char=? x y)))))

(define digit 
  (satisfy char-numeric?))

(define letter
  (satisfy char-alphabetic?))

(define upper-case
  (satisfy char-upper-case?))

(define lower-case
  (satisfy char-lower-case?))

(define alpha-num
  (or-else letter digit))

(define space 
  (satisfy char-whitespace?))

(define digits (many-1 digit))

(define letters (many-1 letter))

(define spaces (many space))

(define trim-left
  (lambda (px)
    (right spaces px)))

(define trim-right
  (lambda (px)
    (left px spaces)))

(define trim
  (lambda (px)
    (between spaces px spaces)))

(define any-of
  (lambda (characters)
    (choice (map character characters))))

(define word
  (lambda (str)
    (map-f list->string (sequence (map character (string->list str))))))
