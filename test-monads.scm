(import (rnrs base)
        (frost data)
        (frost combinators)
        (frost parsers)
        (frost parse)
        (frost utils))

;; R6RS does not support structural equality for record types. "compare" converts 
;; "result" record type to list for deep, structural comparison via "equal?".
(define compare
  (lambda (parse parser text)
    (let* ([result (parse parser text)]
           [flag   (result-flag result)]
           [value  (result-value result)])
      (list flag value))))

;; === Unit Tests: Monads, Alternatives, Functors ===
;;
;; The laws tested in this file are a generally agreed upon set of rules
;; that monads, alternatives, and functors should follow in order to have
;; predictable, common sense behaviour.

;; test data
(define test-data "abc")

;; === Monad Laws ===
;;
;;  return a >>= f = f a <--------------------- Left Unit ---------> unit a * λb.n = n[a/b]
;;    p >>= return = p <----------------------- Right Unit --------> m * λa.unit a = m
;; (p >>= f) >>= g = p >>= (\a -> f a >>= g) <- Associativity -> (m * λa.n) * λb.o = m * (λa.n * λb.o)

;; return a >>= f = f a
;;  unit a * λb.n = n[a/b]
(assert equal?
        (compare parse (bind (return #\a) character) test-data)
        (compare parse (character #\a)               test-data))

;;  p >>= return = p
;; m * λa.unit a = m
(assert equal?
        (compare parse (bind (character #\a) return) test-data)
        (compare parse (character #\a)               test-data))

;;   (p >>= f) >>= g = p >>= (\a -> f a >>= g)
;; (m * λa.n) * λb.o = m * (λa.n * λb.o)
(assert equal?
        (compare parse (bind (bind (return #\a) character) return)                  test-data)
        (compare parse (bind (return #\a) (lambda (x) (bind (character x) return))) test-data))

;; === Alternative and/or MonadPlus Laws ===
;;
;;        zero <|> px = px <------------------ Identity
;;        px <|> zero = px <-
;; px <|> (py <|> pz) = (px <|> py) <|> pz <-- Associativity

;; zero <|> px = px <- Identity
(assert equal?
        (compare parse (or-else zero (character #\a)) test-data)
        (compare parse (character #\a)                test-data))

;; px <|> zero = px <- Identity
(assert equal?
        (compare parse (or-else (character #\a) zero) test-data)
        (compare parse (character #\a)                test-data))

;; px <|> (py <|> pz) = (px <|> py) <|> pz <- Associativity
(assert equal?
        (compare parse (or-else (character #\b) (or-else (character #\a) (character #\c))) test-data)
        (compare parse (or-else (or-else (character #\b) (character #\a)) (character #\c)) test-data))

;; === Functor Laws ===
;;
;; fmap id = id <-------------------- Identity
;; fmap (g ∘ f) = fmap g ∘ fmap f <-- Composition

;; fmap id = id <- Identity
(assert equal?
        (compare parse (fmap identity (character #\a)) test-data)
        (compare parse (character #\a)                 test-data))

;; fmap (g ∘ f) = fmap g ∘ fmap f <- Composition
(let* ([curry-map (lambda (f)
                    (lambda (px) (fmap f px)))]
       [ff (curry-map string-length)]
       [fg (curry-map string)])
  (assert equal?
          (compare parse (fmap (compose string-length string) (character #\a)) test-data)
          (compare parse ((compose ff fg) (character #\a))                     test-data)))
