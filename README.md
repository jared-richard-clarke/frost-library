# Parser Combinators

Parser combinators are extremely interesting but also very complicated. 
It's the monads that confuse me. I'm going to change that.

## About

> "Monadic combinator parsers consist of a monad `Parser a` (typically of the form
>  `String -> Result a` for some functor `Result`) with a unit `return` and bind
>  (`>>=`) operation, and a number of parser specific operations, usually a choice
>  combinator (`<|>`) and a function `satisfy` for constructing elementary parsers
>  for terminal symbols."
>
> — *Parsec: Direct Style Monadic Parser Combinators For The Real World*, by Daan Leijen and Erik Meijer

> "In functional programming, a popular approach to building recursive descent parsers
>  is to model parsers as functions, and to define higher-order functions (or combinators) 
>  that implement grammar constructions such as sequencing, choice, and repetition."
>
>  — *Monadic Parser Combinators*, by Graham Hutton and Erik Meijer

## Future Improvements

1. Implement proper error handling. Parsers currently produce an empty list, `'()`, when failing.
2. Prevent `or-else` (`<|>`) from producing unbound look-ahead.

## Side Note: `apply` or `<*>`

Sequences of `apply` are used to lift multi-parameter functions into a monadic context.
However, this process works best with curried functions because each argument
is applied in its own monadic context. In a strictly-evaluated language like Scheme,
it's simpler to chain a series of `bind`s together and then call the semantic function
at the end of a combinator rather than lift a function into a monadic context piecemeal.

```scheme
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
