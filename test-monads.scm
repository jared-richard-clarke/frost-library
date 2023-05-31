(import (rnrs base)
        (rnrs records syntactic)
        (data)
        (combinators)
        (parsers)
        (utils))

;; Converts `context` record to list for deep structural comparison with `equal?`
(define context->list
  (lambda (ctx)
    (if (not (and (record? ctx) (eq? (record-type-name (record-rtd ctx)) 'context)))
        (error ctx "argument must be a record of type 'context")
        (let* ([reply  (context-reply  ctx)]
               [state  (context-state  ctx)]
               [input  (state-input    state)]
               [line   (state-line     state)]
               [column (state-column   state)]
               [output (context-output ctx)])
          (list reply (list input line column) output)))))

;; === Unit Tests: Monads, Alternatives, Functors ===
;;
;; The laws tested in this file are a generally agreed upon set of rules
;; that monads, alternatives, and functors should follow in order to have
;; predictable, common sense behaviour.

;; test data
(define test-data "abc")

;; === Monad Laws ===
;;
;;  return a >>= f = f a <---------------------- Left Unit ---------> unit a * λb.n = n[a/b]
;;    p >>= return = p <------------------------ Right Unit --------> m * λa.unit a = m
;; (p >>= f) >>= g = p >>= (\a -> f a >>= g) <-- Associativity -> (m * λa.n) * λb.o = m * (λa.n * λb.o)

;; return a >>= f = f a
;;  unit a * λb.n = n[a/b]
(let ([rhs (parse (bind (return #\a) character) test-data)]
      [lhs (parse (character #\a) test-data)])
  (let ([ctx-x (context->list rhs)]
        [ctx-y (context->list lhs)])
    (assert equal? ctx-x ctx-y)))

;;  p >>= return = p
;; m * λa.unit a = m
(let ([rhs (parse (bind (character #\a) return) test-data)]
      [lhs (parse (character #\a) test-data)])
  (let ([ctx-x (context->list rhs)]
        [ctx-y (context->list lhs)])
    (assert equal? ctx-x ctx-y)))

;;   (p >>= f) >>= g = p >>= (\a -> f a >>= g)
;; (m * λa.n) * λb.o = m * (λa.n * λb.o)
(let ([rhs (parse (bind (bind (return #\a) character) return) test-data)]
      [lhs (parse (bind (return #\a) (lambda (x) (bind (character x) return))) test-data)])
  (let ([ctx-x (context->list rhs)]
        [ctx-y (context->list lhs)])
    (assert equal? ctx-x ctx-y)))

;; === Alternative and/or MonadPlus Laws ===
;;
;;        zero <|> px = px <------------------ Identity
;;        px <|> zero = px <-
;; px <|> (py <|> pz) = (px <|> py) <|> pz <-- Associativity

;; zero <|> px = px <- Identity
(let ([rhs (parse (or-else zero (character #\a)) test-data)]
      [lhs (parse (character #\a) test-data)])
  (let ([ctx-x (context->list rhs)]
        [ctx-y (context->list lhs)])
    (assert equal? ctx-x ctx-y)))

;; px <|> zero = px <- Identity
(let ([rhs (parse (or-else (character #\a) zero) test-data)]
      [lhs (parse (character #\a) test-data)])
  (let ([ctx-x (context->list rhs)]
        [ctx-y (context->list lhs)])
    (assert equal? ctx-x ctx-y)))

;; px <|> (py <|> pz) = (px <|> py) <|> pz <- Associativity
(let ([rhs (parse (or-else (character #\b) (or-else (character #\a) (character #\c))) test-data)]
      [lhs (parse (or-else (or-else (character #\b) (character #\a)) (character #\c)) test-data)])
  (let ([ctx-x (context->list rhs)]
        [ctx-y (context->list lhs)])
    (assert equal? ctx-x ctx-y)))

;; === Functor Laws ===
;;
;; fmap id = id <-------------------- Identity
;; fmap (g . f) = fmap g . fmap f <-- Composition

;; fmap id = id <- Identity
(let ([rhs (parse (fmap identity (character #\a)) test-data)]
      [lhs (parse (character #\a) test-data)])
  (let ([ctx-x (context->list rhs)]
        [ctx-y (context->list lhs)])
    (assert equal? ctx-x ctx-y)))

;; fmap (g . f) = fmap g . fmap f <- Composition
(let* ([curry-map (lambda (f)
                    (lambda (px) (fmap f px)))]
       [ff (curry-map string-length)]
       [fg (curry-map string)])
  (let ([rhs (parse (fmap (compose string-length string) (character #\a)) test-data)]
        [lhs (parse ((compose ff fg) (character #\a)) test-data)])
    (let ([ctx-x (context->list rhs)]
          [ctx-y (context->list lhs)])
      (assert equal? ctx-x ctx-y))))
