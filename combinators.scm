;; === UNDER HEAVY CONSTRUCTION ===

(library (combinators)
         ;; === building blocks ===
         (export parse
                 monad-do
                 return
                 bind
                 zero
                 fmap
                 satisfy
         ;; === choices ===
                 or-else
                 try
                 choice
                 option
                 ignore
                 left
                 right
                 between
         ;; === sequences ===
                 and-then
                 sequence
                 many
                 many-1
                 skip-many
                 sep-by
                 sep-by-1
                 end-by
                 end-by-1
                 count)
         (import (rnrs)
                 (data)
                 (utils))

         ;; === BASE ===

         ;; Applies a parser to a string and outputs a parsing context.
         ;;
         ;; A parsing context contains three elements:
         ;;
         ;; 1. reply: An enumeration of '(CONSUMED OK) | '(CONSUMED ERROR) | '(EMPTY OK) | '(EMPTY ERROR).
         ;;    Essential to a consumer-based design as imagined by Dan Leijen and Erik Meijer in
         ;;    "Parsec: Direct Style Monadic Combinators for the Real World".
         ;;
         ;; 2. state: Contains the remaining, unparsed input — a list of characters — as well as parser location
         ;;     — the line and column number — within the input string.
         ;;
         ;; 3. output: An arbitrary value produced by a successful parsing. Produces an empty list on error.

         (define parse
           (lambda (parser text)
             (parser (make-state (string->list text) 1 0))))

         ;; === MONAD ===

         ;; The Haskell "do" syntax (simplified). Makes monads readable.
         ;; Renamed "monad-do" because "do" is less descriptive.

         (define-syntax monad-do
           (syntax-rules (<-)
             [(_ expression) expression]
             [(_ (x <- mx) expression ...)
              (bind mx (lambda (x) 
                         (monad-do expression ...)))]))

         ;; Wraps a value within a parser context.
         ;; Also named "unit". Also named "pure" within the 
         ;; context of applicative functors.

         (define return
           (lambda (x)
             (lambda (state)
               (make-context '(EMPTY OK)
                             state
                             x))))

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
               (let ([ctx-x (px state)])
                 (let ([reply-x  (context-reply  ctx-x)]
                       [state-x  (context-state  ctx-x)]
                       [output-x (context-output ctx-x)])
                   (cond
                     [(equal? reply-x '(EMPTY OK))    ((f output-x) state-x)]
                     [(equal? reply-x '(EMPTY ERROR)) ctx-x]
                     [(equal? reply-x '(CONSUMED OK)) (let ([ctx-y ((f output-x) state-x)])
                                                        (let ([reply-y  (context-reply  ctx-y)]
                                                              [state-y  (context-state  ctx-y)]
                                                              [output-y (context-output ctx-y)])
                                                          (make-context (cons 'CONSUMED (cdr reply-y))
                                                                        state-y
                                                                        output-y)))]
                     [else ctx-x]))))))

         ;; Sets parser context to fail.
         ;; Also named "empty".

         (define zero
           (lambda (state)
             (make-context '(EMPTY ERROR)
                           state
                           '())))

         ;; === functor ===

         ;; Reaches inside a parser and transforms its output with an arbitrary function.

         (define fmap
           (lambda (f px)
             (monad-do (x <- px)
                       (return (f x)))))

         ;; === satisfy ===

         ;; Moves a parser through a string, one character at a time.
         ;; Consumes only values that satisfy an arbitrary predicate.

         (define satisfy
           (lambda (test)
             (lambda (state)
               (let ([input  (state-input  state)]
                     [line   (state-line   state)]
                     [column (state-column state)])
                 (if (empty? input)
                     (make-context '(EMPTY ERROR)
                                   '()
                                   state)
                     (let ([x  (car input)]
                           [xs (cdr input)])
                       (if (test x)
                           (make-context '(CONSUMED OK)
                                         ;; Although #\linefeed and #\newline are synonymous,
                                         ;; older Schemes recognize only #\newline.
                                         (if (or (char=? x #\linefeed) (char=? x #\newline))
                                             (make-state xs (+ line 1) 0)
                                             (make-state xs line (+ column 1)))
                                         x)
                           (make-context '(EMPTY ERROR)
                                         state
                                         '()))))))))

         ;; === choice ===
         ;; side-note: beware of space leaks.

         (define or-else
           (lambda (px py)
             (lambda (state)
               (let ([ctx-x (px state)])
                 (let ([reply-x  (context-reply  ctx-x)]
                       [state-x  (context-state  ctx-x)]
                       [output-x (context-output ctx-x)])
                   (cond
                     [(equal? reply-x '(EMPTY ERROR)) (py state)]
                     [(equal? reply-x '(EMPTY OK))    (let ([ctx-y (py state)])
                                                        (let ([reply-y  (context-reply  ctx-y)]
                                                              [state-y  (context-state  ctx-y)]
                                                              [output-y (context-output ctx-y)])
                                                          (if (eq? (car reply-y) 'EMPTY)
                                                              (make-context '(EMPTY OK)
                                                                            state-y
                                                                            output-y)
                                                              ctx-y)))]
                     [else ctx-x]))))))

         ;; === try: LL(∞) ===

         (define try
           (lambda (px)
             (lambda (state)
               (let ([ctx-x (px state)])
                 (let ([reply-x  (context-reply  ctx-x)]
                       [state-x  (context-state  ctx-x)]
                       [output-x (context-output ctx-x)])
                   (if (equal? reply-x '(CONSUMED ERROR))
                       (make-context '(EMPTY ERROR)
                                     state-x
                                     output-x)
                       ctx-x))))))

         ;; Also named "asum" within the context of Alternatives.
         ;; Also named "msum" within the context of Monads.
         (define choice
           (lambda (parsers)
             (fold-right or-else zero parsers)))

         ;; Applies parser px. If px fails, returns the value y.
         (define option
           (lambda (px y)
             (or-else px (return y))))

         ;; Applies parser px. If px succeeds, ignores its result and returns y.
         (define ignore
           (lambda (px y)
             (monad-do (x <- px)
                       (return y))))


         ;; Also named ".>>", parses two values and discards the right.
         (define left
           (lambda (px py)
             (monad-do (x <- px)
                       (y <- py)
                       (return x))))

         ;; Also named ">>.", parses two values and discards the left.
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

         (define and-then
           (lambda (px py)
             (monad-do (x <- px)
                       (y <- py)
                       (return (cons x y)))))

         (define sequence
           (lambda (parsers)
             (fold-right and-then (return '()) parsers)))

         (define many
           (lambda (px)
             (or-else (many-1 px)
                      (return '()))))

         (define many-1
           (lambda (px)
             (monad-do (x  <- px)
                       (xs <- (many px))
                       (return (cons x xs)))))
         
         ;; (define skip-many
         ;;   (lambda (px)
         ;;     (ignore (many px) '())))
         
         (define skip-many
           (lambda (px)
             (define scan
               (lambda ()
                 (or-else (monad-do (x <- px) (scan))
                          (return '()))))
             (scan)))

         (define sep-by
           (lambda (px sep)
             (or-else (sep-by-1 px sep)
                      (return '()))))

         (define sep-by-1
           (lambda (px sep)
             (monad-do (x  <- px)
                       (xs <- (many (monad-do (s <- sep)
                                              (y <- px)
                                              (return y))))
                       (return (cons x xs)))))

         (define end-by
           (lambda (px sep)
             (many (monad-do (x <- px)
                             (s <- sep)
                             (return x)))))

         (define end-by-1
           (lambda (px sep)
             (many-1 (monad-do (x <- px)
                               (s <- sep)
                               (return x)))))

         (define count
           (lambda (n px)
             (if (<= n 0)
                 (return '())
                 (sequence (repeat n px)))))
         
         )
