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
                 sep-by-1)
         (import (rnrs)
                 (data)
                 (utils))

         ;; === BASE ===

         (define parse
           (lambda (parser text)
             (parser (make-state (string->list text) 1 0))))

         ;; === MONAD ===

         ;; The Haskell "do" syntax (simplified). Makes monads readable.
         ;; Rename "monad-do" because "do" is less descriptive.
         (define-syntax monad-do
           (syntax-rules (<-)
             [(_ expression) expression]
             [(_ (x <- mx) expression ...)
              (bind mx (lambda (x) 
                         (monad-do expression ...)))]))

         ;; Also named "unit". Also called "pure" within the 
         ;; context of Applicative functors.
         (define return
           (lambda (x)
             (lambda (state)
               (make-context EMPTY OK x state))))

         ;; Also named ">>=".
         ;; The binding operation benefits combinator parsing twofold:
         ;; 1. Integrates the sequencing of parsers with the processing of their results.
         ;; 2. Makes the context of previous parsers available to subsequent parsers.
         ;;
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
                 (let ([consumed-x (context-consumed ctx-x)]
                       [reply-x    (context-reply    ctx-x)]
                       [output-x   (context-output   ctx-x)]
                       [state-x    (context-state    ctx-x)])
                   (if (eq? consumed-x EMPTY)
                       (if (eq? reply-x OK)
                           ((f output-x) state-x)
                           ctx-x)
                       (if (eq? reply-x OK)
                           (let ([ctx-y ((f output-x) state-x)])
                             (let ([consumed-y (context-consumed ctx-y)]
                                   [reply-y    (context-reply    ctx-y)]
                                   [output-y   (context-output   ctx-y)]
                                   [state-y    (context-state    ctx-y)])
                               (if (eq? reply-y OK)
                                   (make-context CONSUMED OK output-y state-y)
                                   (make-context CONSUMED ERROR output-y state-y))))
                           ctx-x)))))))

         ;; Also named "empty"
         (define zero
           (lambda (state)
             (make-context EMPTY ERROR '() state)))

         ;; === functor ===

         (define fmap
           (lambda (f px)
             (monad-do (x <- px)
                       (return (f x)))))

         ;; === satisfy ===

         (define satisfy
           (lambda (test)
             (lambda (state)
               (let ([input  (state-input state)]
                     [line   (state-line  state)]
                     [column (state-column state)])
                 (if (empty? input)
                     (make-context EMPTY ERROR '() state)
                     (let ([x  (car input)]
                           [xs (cdr input)])
                       (if (test x)
                           (make-context CONSUMED OK x (make-state xs line (+ column 1)))
                           (make-context EMPTY ERROR '() state))))))))

         ;; === choice ===
         ;; side-note: beware of space leaks.

         (define or-else
           (lambda (px py)
             (lambda (state)
               (let ([ctx-x (px state)])
                 (let ([consumed-x (context-consumed ctx-x)]
                       [reply-x    (context-reply    ctx-x)]
                       [output-x   (context-output   ctx-x)]
                       [state-x    (context-state    ctx-x)])
                   (if (eq? consumed-x EMPTY)
                       (let ([ctx-y (py state)])
                         (let ([consumed-y (context-consumed ctx-y)]
                               [reply-y    (context-reply    ctx-y)]
                               [output-y   (context-output   ctx-y)]
                               [state-y    (context-state    ctx-y)])
                           (if (eq? reply-x ERROR)
                               (if (eq? consumed-y EMPTY)
                                   (if (eq? reply-y ERROR)
                                       (make-context EMPTY ERROR '() state-y)
                                       (make-context EMPTY OK output-y state-y))
                                   ctx-y)
                               (if (eq? consumed-y EMPTY)
                                   (if (eq? reply-y ERROR)
                                       (make-context EMPTY ERROR output-x state-x)
                                       (make-context EMPTY OK output-x state-x))
                                   ctx-y)))) 
                       ctx-x))))))

         ;; === try: LL(∞) ===

         (define try
           (lambda (px)
             (lambda (state)
               (let ([ctx-x (px state)])
                 (let ([consumed-x (context-consumed ctx-x)]
                       [reply-x    (context-reply    ctx-x)]
                       [output-x   (context-output   ctx-x)]
                       [state-x    (context-state    ctx-x)])
                   (if (and (eq? consumed-x CONSUMED) (eq? reply-x ERROR))
                       (make-context EMPTY ERROR output-x state-x)
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
           (lambda parsers
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

         ;; (define many-1
         ;;   (lambda (px)
         ;;     (bind px (lambda (x)
         ;;                (bind (many px) (lambda (xs)
         ;;                                 (return (cons x xs))))))))
         
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
         
         )
