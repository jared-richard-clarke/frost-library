# Parser Combinators

I'm building this parser combinator library to better understand parsing, 
functional programming, and monads.

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

1. Implement proper error messaging. Parsers currently produce an empty list, `'()`, when failing.
2. Restrict lookahead, **LL(1)**, for `(or-else px py)`. If `px` fails after consuming input, `py` should fail
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

3. Parameterize input. Allow for inputs other than strings.

## Side Note: `apply` or `<*>`

Sequences of applicatives are used to lift multi-parameter functions into a monadic context.
However, this process works best with curried functions because each argument is applied
in its own monadic context. In a language like Scheme, where currying is possible but not automatic,
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
