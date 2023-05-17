(import (rnrs)
        (combinators)
        (parsers)
        (utils))

;; Converts `context` record to list for deep structural comparison with `equal?`
(define context->list
  (lambda (ctx)
    (if (not (and (record? ctx) (eq? (record-type-name (record-rtd ctx)) 'context)))
        (error ctx "argument must be a record of type 'context")
        (let* ([reply  (context-reply ctx)]
               [state  (context-state ctx)]
               [input  (state-input state)]
               [line   (state-line  state)]
               [column (state-column state)]
               [output (context-output ctx)])
          (list reply (list input line column) output)))))

;; === Unit Tests: Monads, Alternatives, Functors ===
;;
;; The laws tested in this file are a generally agreed upon set of rules
;; that monads, alternatives, and functors should follow in order to have
;; predictable, common sense behaviour.
;;
;; Side Note: Cannot compare functions directly. Must compare their outputs.
;; In a purely functional language, there are only inputs and outputs.

;; test data
;; ...

;; === Monad Laws ===
;;
;;  return a >>= f = f a <---------------------- Left Unit ---------> unit a * λb.n = n[a/b]
;;    p >>= return = p <------------------------ Right Unit --------> m * λa.unit a = m
;; (p >>= f) >>= g = p >>= (\a -> f a >>= g) <-- Associativity -> (m * λa.n) * λb.o = m * (λa.n * λb.o)

;; return a >>= f = f a
;;  unit a * λb.n = n[a/b]
;; ...

;;  p >>= return = p
;; m * λa.unit a = m
;; ...

;;   (p >>= f) >>= g = p >>= (\a -> f a >>= g)
;; (m * λa.n) * λb.o = m * (λa.n * λb.o)
;; ...

;; === Alternative and/or MonadPlus Laws ===
;;
;;        zero <|> px = px <------------------ Identity
;;        px <|> zero = px <-
;; px <|> (py <|> pz) = (px <|> py) <|> pz <-- Associativity

;; zero <|> px = px <|> zero <- Identity
;; ...

;; px <|> (py <|> pz) = (px <|> py) <|> pz <- Associativity
;; ...

;; === Functor Laws ===
;;
;; fmap id = id <-------------------- Identity
;; fmap (g . f) = fmap g . fmap f <-- Composition

;; fmap id = id <- Identity
;; ...

;; fmap (g . f) = fmap g . fmap f <- Composition
;; ...
