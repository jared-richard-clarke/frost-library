# Frost: a parser combinator library

I'm building an experimental parser combinator library to better understand parsing, 
functional programming, and monads.

## About

> "In functional programming, a popular approach to building recursive descent parsers
>  is to model parsers as functions, and to define higher-order functions (or combinators) 
>  that implement grammar constructions such as sequencing, choice, and repetition."
>
>  — *Monadic Parser Combinators*, by Graham Hutton and Erik Meijer

## Future Improvements

- [x] Implement user-friendly error messaging. Currently provides state at location of failure.
- [x] Restrict lookahead, **LL(1)**, for `(or-else px py)`. If `px` fails after consuming input, `py` should fail
   without consuming any input. Some grammars, however, require arbitrary lookahead. Lookahead should
   be allowed if required explicitly.

> Naive implementations of backtracking parser combinators suffer from a space leak.
> The problem originates in the definition of the `choice` combinator. It either
> always tries its second alternative (because it tries to find all possible parses),
> or whenever the first alternative fails (because it requires arbitrary lookahead).
> As a result, the parser `p <|> q` holds on to its input until `p` returns, since it
> needs the original input to run parser `q` when `p` has failed. The space leak leads
> quickly to either a stack/heap overflow or reduction in speed on larger inputs.
>
> — *Parsec: Direct Style Monadic Parser Combinators For The Real World*
>   by Daan Leijen and Erik Meijer

- [ ] Parameterize input. Allow for inputs other than strings.

## Side Note About Applicatives: `apply` or `<*>`

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
