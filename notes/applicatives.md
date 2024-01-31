# Side Note About Applicatives: `apply` or `<*>`

Sequences of `apply` are used to lift multi-parameter functions into an applicative context piecemeal.
This technique requires functions to be curried and is most conveniently realized in languages that
curry their functions automatically. It is simpler in Scheme to first unwrap each value from their
respective context and then apply the semantic function.

```scheme
;; Applicative defined in a monadic context.
;; Applying multi-parameter function "f" to a single argument is impossible in Scheme.

(define apply-p
  (lambda (pf px)
    (monad-do (f <- pf)
              (fmap f px))))
              
;; === or ====

(define apply-p
  (lambda (pf px)
    (monad-do (f <- pf)
              (x <- px)
              (return (f x)))))
```
