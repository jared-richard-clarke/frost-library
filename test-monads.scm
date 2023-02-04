(import (rnrs)
        (parser-combinators)
        (utils))

;; === Unit Tests: Monads, Alternatives, Functors ===
;;
;; The laws tested in this file are a generally agreed upon set of rules
;; that monads, alternatives, and functors should follow in order to have
;; predictable, common sense behaviour.
;;
;; Side Note: Cannot compare functions directly. Must compare their outputs.
;; In a purely functional language, there are only inputs and outputs.
;; Functions are black boxes.

;; test data
(define abc (string->list "abc"))

;; === Monad Laws ===
;;
;;            return a >>= f = f a <------------------------ Left identity
;;              p >>= return = p <-------------------------- Right identity
;; p >>= (\a -> (f a >>= g)) = (p >>= (\a -> f a)) >>= g <-- Associativity

;; return a >>= f = f a
(define test-a-lhs (bind (return #\a) character))
(define test-a-rhs (character #\a))

(assert-test equal? (test-a-lhs abc) (test-a-rhs abc))

;; p >>= return = p
(define test-b-lhs (bind item return))
(define test-b-rhs item)

(assert-test equal? (test-b-lhs abc) (test-b-rhs abc))

;; p >>= (\a -> (f a >>= g)) = (p >>= (\a -> f a)) >>= g
;;
;; Associativity for monads is a tricky problem. One I have yet to fully understand.

;; === Alternative and/or MonadPlus Laws ===
;;
;;        zero <|> px = px <------------------ Identity
;;        px <|> zero = px <-
;; px <|> (py <|> pz) = (px <|> py) <|> pz <-- Associativity

;; zero <|> px = px <|> zero
(define test-d-lhs (or-else zero (character #\a)))
(define test-d-rhs (or-else (character #\a) zero))

(assert-test equal? (test-d-lhs abc) (test-d-rhs abc))

;; px <|> (py <|> pz) = (px <|> py) <|> pz
(define test-e-lhs (or-else (character #\a) (or-else (character #\b) (character #\c))))
(define test-e-rhs (or-else (or-else (character #\a) (character #\b)) (character #\c)))

(assert-test equal? (test-e-lhs abc) (test-e-rhs abc))

;; === Functor Laws ===
;;
;; fmap id = id <-------------------- Identity
;; fmap (g . f) = fmap g . fmap f <-- Composition

;; fmap id = id
(define test-f-lhs (map-f (lambda (x) x) (character #\a)))
(define test-f-rhs (lambda (x) x))

(assert-test equal? (test-f-lhs abc) (test-f-rhs ((character #\a) abc)))

;; fmap (g . f) = fmap g . fmap f
(define test-g-lhs (map-f (compose string-length string) (character #\a)))
(define test-g-rhs
  (let* ([curry-map (lambda (f)
                      (lambda (px) (map-f f px)))]
         [fg (curry-map string-length)]
         [ff (curry-map string)])
    ((compose fg ff) (character #\a))))

(assert-test equal? (test-g-lhs abc) (test-g-rhs abc))
