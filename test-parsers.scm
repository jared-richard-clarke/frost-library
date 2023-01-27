(import (rnrs)
        (parsers)
        (utils))

;; === Unit Tests ===
;;
;; Side Note: Cannot compare functions directly. Must compare their outputs.
;; In a purely functional language, there are only inputs and outputs.
;; Functions are black boxes.

;; test data
(define abc (string->list "abc"))

;; === Monad Laws ===
;;
;;            return a >>= f = f a <------------------------ Right argument to "bind" involves a binding operation.
;;              p >>= return = p <-------------------------- "return" is a right and left unit for "bind".
;; p >>= (\a -> (f a >>= g)) = (p >>= (\a -> f a)) >>= g <-- "bind" is associative.

;; return a >>= f = f a
;;              ^---^ Must be a monad-producing function.
(define test-a-lhs
  (bind (return '()) return))

(define test-a-rhs
  (return '()))

(assert-test equal? (test-a-lhs abc) (test-a-rhs abc))

;; p >>= return = p
;;       ^ Must be a monad-producing function.
(define test-b-lhs (bind item return))
(define test-b-rhs item)

(assert-test equal? (test-b-lhs abc) (test-b-rhs abc))

;; p >>= (\a -> (f a >>= g)) = (p >>= (\a -> f a)) >>= g
;;               ^-------^-------------------^---------^ Must be monad-producing functions.
(define test-c-lhs
  (bind item (lambda (a)
               (bind (return a) return))))

(define test-c-rhs
  (bind (bind item return) return))

(assert-test equal? (test-c-lhs abc) (test-c-rhs abc))

;; === Alternative and/or MonadPlus Laws ===
;;
;;        zero <|> px = px <------------------ "zero" is neutral.
;;        px <|> zero = px <-
;; px <|> (py <|> pz) = (px <|> py) <|> pz <-- "(<|>)" is associative

;; zero <|> px = px <|> zero
(define test-d-lhs (or-else zero (character #\a)))
(define test-d-rhs (or-else (character #\a) zero))

(assert-test equal? (test-d-lhs abc) (test-d-rhs abc))

;; px <|> (py <|> pz) = (px <|> py) <|> pz
(define test-e-lhs (or-else (character #\a) (or-else (character #\b) (character #\c))))
(define test-e-rhs (or-else (or-else (character #\a) (character #\b)) (character #\c)))

(assert-test equal? (test-e-lhs abc) (test-e-rhs abc))
