# Parser Combinators

Parser combinators are extremely interesting but also very complicated. 
It's the monads that confuse me. I'm going to change that.

## About

> "Monadic combinator parsers consist of a monad `Parser` a (typically of the form
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
