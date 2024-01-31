# Frost: a parser combinator library

I built an experimental parser combinator library to better understand parsing, 
functional programming, and monads.

> "In functional programming, a popular approach to building recursive descent parsers
>  is to model parsers as functions, and to define higher-order functions (or combinators) 
>  that implement grammar constructions such as sequencing, choice, and repetition."
>
>  — *Monadic Parser Combinators*, Graham Hutton and Erik Meijer

## Sources

### Overview

| source                                                                 | author                                         |
|----------------------------------------------------------------------- | ---------------------------------------------- |
| **Monads for Functional Programming**                                  | Philip Wadler                                  |
| **Monadic Parser Combinators**                                         | Graham Hutton and Erik Meijer                  |
| **Monadic Parsing in Haskell**                                         | Graham Hutton and Erik Meijer                  |
| **Parsec: Direct Style Monadic Parser Combinators For The Real World** | Daan Leijen and Erik Meijer                    |
| **Understanding Parser Combinators**                                   | Scott Wlaschin                                 |
| **Parsec: Monadic Parser Combinators**                                 | Daan Leijen, Paolo Martini, and Antoine Latter |

### Details

- P. Wadler, "Monads for functional programming." Available: https://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf

- G. Hutton and E. Meijer, "Monadic Parser Combinators." Available: https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf

- G. Hutton and E. Meijer, "Monadic parsing in Haskell," Journal of Functional Programming,
  vol. 8, no. 4, pp. 437–444, Jul. 1998, doi: https://doi.org/10.1017/s0956796898003050.

- D. Leijen and E. Meijer, "Parsec: Direct Style Monadic Parser Combinators For The Real World," 2001.
  Available: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/parsec-paper-letter.pdf

- S. Wlaschin, "Understanding Parser Combinators | F# for fun and profit."
  Available: https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/ (accessed Jan. 30, 2024).

- D. Leijen, P. Martini, and A. Latter, "Parsec: Monadic Parser Combinators" (3.1.17.0).
  Available: https://hackage.haskell.org/package/parsec (accessed Jan. 30, 2024).

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
