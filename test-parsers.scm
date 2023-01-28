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
(define test-a-lhs (bind (return '()) return))
(define test-a-rhs (return '()))

(assert-test equal? (test-a-lhs abc) (test-a-rhs abc))

;; p >>= return = p
(define test-b-lhs (bind item return))
(define test-b-rhs item)

(assert-test equal? (test-b-lhs abc) (test-b-rhs abc))

;; p >>= (\a -> (f a >>= g)) = (p >>= (\a -> f a)) >>= g
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

;;                zero >>= f = zero
;;          p >>= const zero = zero
;;           (p <|> q) >>= f = (p >>= f) <|> (q >>= f)
;; p >>= (\a -> f a <|> g a) = (p >>= f) <|> (p >>= g)

;; zero >>= f = zero
(define test-f-lhs (bind zero return))
(define test-f-rhs zero)

(assert-test equal? (test-f-lhs abc) (test-f-rhs abc))

;; p >>= const zero = zero
(define test-g-lhs (bind (character #\a) (lambda (x) zero)))
(define test-g-rhs zero)

(assert-test equal? (test-g-lhs abc) (test-g-rhs abc))

;; (p <|> q) >>= f = (p >>= f) <|> (q >>= f)
(define test-h-lhs (bind (or-else (character #\a) (character #\b)) return))
(define test-h-rhs (or-else (bind (character #\a) return) (bind (character #\b) return)))

(assert-test equal? (test-h-lhs abc) (test-h-rhs abc))

;; p >>= (\a -> f a <|> g a) = (p >>= f) <|> (p >>= g)
(define test-i-lhs (bind (character #\a) (lambda (a) (or-else (return a) (return a)))))
(define test-i-rhs (or-else (bind (character #\a) return) (bind (character #\a) return)))

(assert-test equal? (test-i-lhs abc) (test-i-rhs abc))
