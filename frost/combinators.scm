(library (frost combinators)
         ;; === building blocks ===
         (export monad-do
                 <-
                 return
                 bind
                 empty
                 fail
                 fmap
                 satisfy
                 try
                 label
         ;; === choices ===
                 choice
                 maybe
                 either
                 replace
                 left
                 right
                 between
         ;; === sequences ===
                 sequence
                 many
                 many-1
                 skip-many
                 sep-by
                 sep-by-1
                 end-by
                 end-by-1
                 chain-left
                 chain-left-1
                 chain-right
                 chain-right-1)
         (import (rnrs base)
                 (rnrs lists)
                 (rnrs unicode)
                 (frost data)
                 (frost utils))

         ;; === MONAD DO SYNTAX ===

         ;; The Haskell "do" syntax (simplified). Makes monads readable.
         ;; Renamed "monad-do" because "do" is less descriptive.
         (define-syntax monad-do
           (syntax-rules (<-)
             [(_ expression) expression]
             [(_ (x <- mx) expression ...)
              (bind mx (lambda (x) 
                         (monad-do expression ...)))]))

         ;; It is a syntax violation to use "<-" for any purpose
         ;; other than as an auxiliary keyword for "monado-do".
         (define-syntax <-
           (lambda (x)
             (syntax-violation '<- "misplaced auxiliary keyword" x)))

         ;; === MONAD ===
         ;;          |---------------------------------|
         ;; parser = state -> reply, state, want, output
         
         ;; Wraps a value within a parser. The identity for sequencing operations.
         (define return
           (lambda (x)
             (lambda (state)
               (values EMPTY-OK state NONE x))))

         ;; Also named ">>=".
         ;; The binding operation benefits combinator parsing twofold:
         ;; 1. Integrates the sequencing of parsers with the processing of their results.
         ;; 2. Makes the context of previous parsers available to subsequent parsers.
         ;;
         ;; Input Consumption of (>>=) for Parsers "p" and "q".
         ;; Side-Note: essentially behaves like Boolean "or".
         ;;
         ;; | p        | q        | (p >>= q) |
         ;; | -------- | -------- | --------- |
         ;; | Empty    | Empty    | Empty     |
         ;; | Empty    | Consumed | Consumed  |
         ;; | Consumed | Empty    | Consumed  |
         ;; | Consumed | Consumed | Consumed  |
         (define bind
           (lambda (px f)
             (lambda (state)
               (let-values ([(reply-x state-x want-x output-x) (px state)])
                 (cond
                   [(equal? reply-x EMPTY-OK)    ((f output-x) state-x)]
                   [(equal? reply-x EMPTY-ERROR) (values reply-x state-x want-x output-x)]
                   [(equal? reply-x CONSUMED-OK) (let-values ([(reply-y state-y want-y output-y)
                                                               ((f output-x) state-x)])
                                                   (values (cons CONSUMED (cdr reply-y)) state-y want-y output-y))]
                   [else (values reply-x state-x want-x output-x)])))))

         ;; Sets parser context to an empty but successful state.
         ;; The identity for sequencing operations.
         (define empty (return '()))

         ;; Sets parser context to a failing state. The identity for choice operations.
         (define fail
           (lambda (state)
             (values EMPTY-ERROR state NONE NONE)))
         
         ;; === functor ===

         ;; Reaches inside a parser and transforms its output with an arbitrary function.
         (define fmap
           (lambda (f px)
             (monad-do (x <- px)
                       (return (f x)))))

         ;; === satisfy ===

         ;; Provides a parser that only consumes its input if that input satisfies
         ;; the provided predicate.
         (define satisfy
           (lambda (test)
             (lambda (state)
               (let ([input  (state-input  state)]
                     [length (state-length state)]
                     [offset (state-offset state)]
                     [line   (state-line   state)]
                     [column (state-column state)])
                 (if (= offset length)
                     (fail state)
                     (let ([x (vector-ref input offset)])
                       (if (test x)
                           (values CONSUMED-OK
                                   ;; Although #\linefeed and #\newline are synonymous,
                                   ;; older Schemes recognize only #\newline
                                   (if (or (char=? x #\linefeed) (char=? x #\newline))
                                       (make-state input length (+ offset 1) (+ line 1) 0)
                                       (make-state input length (+ offset 1) line (+ column 1)))
                                   NONE
                                   x)
                           (fail state))))))))

         ;; === try: LL(∞) ===

         ;; Parser "(try px)" behaves exactly like parser "px" except
         ;; that it pretends it hasn't consumed any input if "px" fails.
         ;; This allows combinator "or-else" unlimited lookahead.
         (define try
           (lambda (px)
             (lambda (state)
               (let-values ([(reply-x state-x want-x output-x) (px state)])
                 (if (equal? reply-x CONSUMED-ERROR)
                     (values EMPTY-ERROR state-x want-x output-x)
                     (values reply-x     state-x want-x output-x))))))

         ;; Parser "(label message px)" behaves exactly like parser "px", but
         ;; when it fails without consuming input, it sets "want-x" to "message".
         (define label
           (lambda (message px)
             (lambda (state)
               (let-values ([(reply-x state-x want-x output-x) (px state)])
                 (if (eq? (car reply-x) EMPTY)
                     (values reply-x state-x (list message) output-x)
                     (values reply-x state-x want-x output-x))))))
         
         ;; === choice ===
         ;; side-note: Beware of space leaks via catastrophic backtracking.

         ;; Applies parser "px" to the input. If "px" succeeds, parser "py" is ignored.
         ;; If "px" fails, "py" is applied to the same input and its result is outputted
         ;; regardless of whether it succeeds or fails.
         ;; To prevent catastrophic backtracking, "or-else" ignores parser "py" if
         ;; parser "px" consumes any input prior to failing. This behavior can be
         ;; inverted with the "try" combinator.
         (define or-else
           (lambda (px py)
             (lambda (state)
               (let-values ([(reply-x state-x want-x output-x) (px state)])
                 (cond
                   [(equal? reply-x EMPTY-ERROR)
                    (let-values ([(reply-y state-y want-y output-y) (py state)])
                      (if (eq? (car reply-y) EMPTY)
                          (values reply-y state-y (append want-x want-y) output-y)
                          (values reply-y state-y want-y output-y)))]
                   [(equal? reply-x EMPTY-OK)
                    (let-values ([(reply-y state-y want-y output-y) (py state)])
                      (if (eq? (car reply-y) EMPTY)
                          (values EMPTY-OK state-y (append want-x want-y) output-y)
                          (values reply-y state-y want-y output-y)))]
                   [else (values reply-x state-x want-x output-x)])))))


         ;; Applies each parser in a list, outputting the result of the first successful parser.
         ;; Also named "asum" within the context of Alternatives.
         ;; Also named "msum" within the context of Monads.
         (define choice
           (lambda parsers
             (fold-right or-else fail parsers)))
         
         ;; Applies parser px. Returns the result of px if px succeeds.
         ;; Returns an empty result if px fails.
         (define maybe
           (lambda (px)
             (or-else px empty)))

         ;; Applies parser px. Returns the result of px if px succeeds.
         ;; Returns arbitrary value y if px fails.
         (define either
           (lambda (px y)
             (or-else px (return y))))

         ;; Applies parser px. If px succeeds, replaces its result with value y.
         (define replace
           (lambda (px y)
             (monad-do (x <- px)
                       y)))

         ;; Also named ".>>", "left" parses two values and discards the right.
         (define left
           (lambda (px py)
             (monad-do (x <- px)
                       (y <- py)
                       (return x))))

         ;; Also named ">>.", "right" parses two values and discards the left.
         (define right
           (lambda (px py)
             (monad-do (x <- px)
                       (y <- py)
                       (return y))))

         ;; Parses three values, and, if successful, discards the left and the right values.
         (define between
           (lambda (px py pz)
             (monad-do (x <- px)
                       (y <- py)
                       (z <- pz)
                       (return y))))

         ;; === sequence ===

         ;; Applies parsers "px" and "py", only outputting a pair of values if both parsers succeed.
         (define and-then
           (lambda (px py)
             (monad-do (x <- px)
                       (y <- py)
                       (return (cons x y)))))

         ;; Applies a list of parsers, only outputting a list of values if all parsers succeed.
         (define sequence
           (lambda parsers
             (fold-right and-then empty parsers)))

         ;; Applies a parser zero or more times. Outputs a list of zero or more parsed values.
         (define many
           (lambda (px)
             (or-else (many-1 px)
                      empty)))

         ;; Applies a parser one or more times. Only outputs a list of one or more parsed values.
         (define many-1
           (lambda (px)
             (monad-do (x  <- px)
                       (xs <- (many px))
                       (return (cons x xs)))))

         ;; Applies a parser zero or more times, ignoring the result.
         (define skip-many
           (lambda (px)
             (define scan
               (lambda ()
                 (or-else (monad-do (x <- px) (scan))
                          empty)))
             (scan)))

         ;; Parses zero or more occurrences of parser "px" separated by parser "sep".
         ;; Outputs a list of zero or more parsed values.
         (define sep-by
           (lambda (px sep)
             (or-else (sep-by-1 px sep)
                      empty)))

         ;; Parses one or more occurrences of parser "px" separated by parser "sep".
         ;; Only outputs a list of one or more parsed values.
         (define sep-by-1
           (lambda (px sep)
             (monad-do (x  <- px)
                       (xs <- (many (monad-do (s <- sep)
                                              (y <- px)
                                              (return y))))
                       (return (cons x xs)))))

         ;; Parses zero or more occurrences of parser "px" separated and ended by parser "sep".
         ;; Outputs a list of zero or more parsed values.
         (define end-by
           (lambda (px sep)
             (many (monad-do (x <- px)
                             (s <- sep)
                             (return x)))))

         ;; Parses one or more occurrences of parser "px" separated and ended by parser "sep".
         ;; Only outputs a list of one or more parsed values.
         (define end-by-1
           (lambda (px sep)
             (many-1 (monad-do (x <- px)
                               (s <- sep)
                               (return x)))))

         ;; Same behavior as "chain-left-1" except a given value "v" is returned for empty sequences.
         (define chain-left
           (lambda (px op v)
             (or-else (chain-left-1 px op)
                      (return v))))

         ;; === chain: repetition with meaningful separators. ===

         ;; Parses and evaluates non-empty sequences of items separated by operators that associate left.
         (define chain-left-1
           (lambda (px op)
             (letrec ([rest (lambda (a)
                              (or-else (monad-do (f <- op)
                                                 (b <- px)
                                                 (rest (f a b)))
                                       (return a)))])
               (monad-do (x <- px)
                         (rest x)))))

         ;; Same behavior as "chain-right-1" except a given value "v" is returned for empty sequences.
         (define chain-right
           (lambda (px op v)
             (or-else (chain-right-1 px op)
                      (return v))))

         ;; Parses and evaluates non-empty sequences of items separated by operators that associate right.
         (define chain-right-1
           (lambda (px op)
             (monad-do (x <- px)
                       (or-else (monad-do (f <- op)
                                          (y <- (chain-right-1 px op))
                                          (return (f x y)))
                                (return x)))))

         )
