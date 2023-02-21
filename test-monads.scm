(import (rnrs)
        (combinators)
        (parsers)
        (utils))

;; === Unit Tests: Monads, Alternatives, Functors ===
;;
;; The laws tested in this file are a generally agreed upon set of rules
;; that monads, alternatives, and functors should follow in order to have
;; predictable, common sense behaviour.
;;
;; Side Note: Cannot compare functions directly. Must compare their outputs.
;; In a purely functional language, there are only inputs and outputs.

;; test data
(define abc (string->list "abc"))

;; === Monad Laws ===
;;
;;            return a >>= f = f a <---------------------- Left Unit ---------> unit a * λb.n = n[a/b]
;;              p >>= return = p <------------------------ Right Unit --------> m * λa.unit a = m
;;           (p >>= f) >>= g = p >>= (\a -> f a >>= g) <-- Associativity -> (m * λa.n) * λb.o = m * (λa.n * λb.o)

;; return a >>= f = f a
;;  unit a * λb.n = n[a/b]
(define test-a-lhs (bind (return #\a) character))
(define test-a-rhs (character #\a))

(assert-test equal? (test-a-lhs abc) (test-a-rhs abc))

;;  p >>= return = p
;; m * λa.unit a = m
(define test-b-lhs (bind (character #\a) return))
(define test-b-rhs (character #\a))

(assert-test equal? (test-b-lhs abc) (test-b-rhs abc))

;;   (p >>= f) >>= g = p >>= (\a -> f a >>= g)
;; (m * λa.n) * λb.o = m * (λa.n * λb.o)
(define test-c-lhs
  (bind (bind (character #\a) (lambda (x) (character #\b))) (lambda (y) (return y))))
(define test-c-rhs
  (bind (character #\a) (lambda (x) (bind (character #\b) (lambda (y) (return y))))))

(assert-test equal? (test-c-lhs abc) (test-c-rhs abc))

;; === Alternative and/or MonadPlus Laws ===
;;
;;        zero <|> px = px <------------------ Identity
;;        px <|> zero = px <-
;; px <|> (py <|> pz) = (px <|> py) <|> pz <-- Associativity

;; zero <|> px = px <|> zero <- Identity
(define test-d-lhs (or-else zero (character #\a)))
(define test-d-rhs (or-else (character #\a) zero))

(assert-test equal? (test-d-lhs abc) (test-d-rhs abc))

;; px <|> (py <|> pz) = (px <|> py) <|> pz <- Associativity
(define test-e-lhs (or-else (character #\a) (or-else (character #\b) (character #\c))))
(define test-e-rhs (or-else (or-else (character #\a) (character #\b)) (character #\c)))

(assert-test equal? (test-e-lhs abc) (test-e-rhs abc))

;; === Functor Laws ===
;;
;; fmap id = id <-------------------- Identity
;; fmap (g . f) = fmap g . fmap f <-- Composition

;; fmap id = id <- Identity
(define test-f-lhs (map-f (lambda (x) x) (character #\a)))
(define test-f-rhs (lambda (x) x))

(assert-test equal? (test-f-lhs abc) (test-f-rhs ((character #\a) abc)))

;; fmap (g . f) = fmap g . fmap f <- Composition
(define test-g-lhs (fmap (compose string-length string) (character #\a)))
(define test-g-rhs
  (let ([curry-map (lambda (f)
                     (lambda (px) (fmap f px)))])
    (let ([fg (curry-map string-length)]
          [ff (curry-map string)])
      ((compose fg ff) (character #\a)))))

(assert-test equal? (test-g-lhs abc) (test-g-rhs abc))
