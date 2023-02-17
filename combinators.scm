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
                 sep-by
                 sep-by-1)
         (import (rnrs)
                 (utils))

         ;; === BASE ===

         (define parse
           (lambda (parser text)
             (parser (string->list text))))

         ;; === MONAD ===

         ;; The Haskell "do" syntax (simplified). Makes monads readable.
         ;; Rename "monad-do" because "do" is less descriptive.
         (define-syntax monad-do
           (lambda (stx)
             (syntax-case stx (<-)
               [(_ expression)
                (syntax expression)]
               [(_ (x <- mx) expression ...)
                (syntax (bind mx (lambda (x) 
                                   (monad-do expression ...))))])))

         ;; Also named "unit". Also called "pure" within the 
         ;; context of Applicative functors.
         (define return
           (lambda (x)
             (lambda (text)
               (make-context EMPTY OK x text))))

         ;; Also named ">>=".
         ;; In this context, integrates the sequencing of parsers 
         ;; with the processing of their results.
         (define bind
           (lambda (px f)
             (lambda (text)
               (let ([ctx-x (px text)])
                 (let ([consumed (context-consumed ctx-x)]
                       [reply    (context-reply    ctx-x)]
                       [output   (context-output   ctx-x)]
                       [input    (context-input    ctx-x)])
                   (if (eq? consumed EMPTY)
                       (if (eq? reply OK)
                           ((f output) input)
                           (make-context EMPTY ERROR output input))
                       (if (eq? reply OK)
                           (let ([ctx-y ((f output) input)])
                             (let ([consumed (context-consumed ctx-y)]
                                   [reply    (context-reply    ctx-y)]
                                   [output   (context-output   ctx-y)]
                                   [input    (context-input    ctx-y)])
                               (if (eq? reply OK)
                                   (make-context CONSUMED OK output input)       ;; <- lazy eval: how?
                                   (make-context CONSUMED ERROR output input)))) ;; <- 
                           ctx-x)))))))

         ;; Also named "empty"
         (define zero
           (lambda (text)
             (make-context EMPTY ERROR '() text)))

         ;; === functor ===

         (define fmap
           (lambda (f px)
             (monad-do (x <- px)
                       (return (f x)))))

         ;; === satisfy ===

         (define satisfy
           (lambda (test)
             (lambda (text)
               (if (empty? text)
                   (make-context EMPTY ERROR '() text)
                   (let ([x  (car text)]
                         [xs (cdr text)])
                     (if (test x)
                         (make-context CONSUMED OK x xs)
                         (make-context EMPTY ERROR '() text)))))))

         ;; === choice ===
         ;; side-note: beware of space leaks.
         (define or-else
           (lambda (px py)
             (lambda (text)
               (let ([ctx-x (px text)])
                 (let ([consumed (context-consumed ctx-x)]
                       [reply    (context-reply    ctx-x)]
                       [output   (context-output   ctx-x)]
                       [input    (context-input    ctx-x)])
                   (if (eq? consumed EMPTY)
                       (if (eq? reply ERROR)
                           (py text)
                           (let ([ctx-y (py text)])
                             (let ([consumed (context-consumed ctx-y)]
                                   [reply    (context-reply    ctx-y)]
                                   [output   (context-output   ctx-y)]
                                   [input    (context-input    ctx-y)])
                               (if (eq? consumed EMPTY)
                                   (make-context EMPTY OK output input)
                                   ctx-y))))
                       ctx-x))))))

         (define choice
           (lambda (parsers)
             (fold-right or-else zero parsers)))

         ;; Applies parser px. If px fails, returns the value y.
         (define option
           (lambda (px y)
             (or-else px (return y))))

         ;; Applies parser px. If px succeeds, ignore its result and return y.
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

         ;; === try: LL(∞) ===

         (define try
           (lambda (px)
             (lambda (text)
               (let ([ctx-x (px text)])
                 (let ([consumed (context-consumed ctx-x)]
                       [reply    (context-reply    ctx-x)]
                       [output   (context-output   ctx-x)]
                       [input    (context-input    ctx-x)])
                   (if (and (eq? consumed CONSUMED) (eq? reply ERROR))
                       (make-context EMPTY ERROR output input)
                       ctx-x))))))

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

         ;; (define many-1
         ;;   (lambda (px)
         ;;     (bind px (lambda (x)
         ;;                (bind (many px) (lambda (xs)
         ;;                                 (return (cons x xs))))))))

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
