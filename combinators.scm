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
         
         ;; (parser (list char)) -> (list) | (list any (list char))

         ;; === base ===

         (define parse
           (lambda (parser text)
             (parser (string->list text))))
         
         ;; === monad ====

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
             (lambda (input)
               (list x input))))

         ;; Also named ">>=".
         ;; In this context, integrates the sequencing of parsers 
         ;; with the processing of their results.
         (define bind
           (lambda (px f)
             (lambda (input)
               (let ([x (px input)])
                 (if (empty? x)
                     x
                     ((f (car x)) (cadr x)))))))

         ;; Also named "empty"
         (define zero (lambda input '()))

         ;; === functor ===

         (define fmap
           (lambda (f px)
             (monad-do (x <- px)
                       (return (f x)))))

         ;; === satisfy ===

         (define satisfy
           (lambda (test)
             (lambda (input)
               (if (empty? input)
                   '()
                   (let ([x  (car input)]
                         [xs (cdr input)])
                     (if (not (test x))
                         '()
                         (list x xs)))))))

         ;; === choices ===

         (define or-else
           (lambda (px py)
             (lambda (input)
               (let ([x (px input)])
                 (if (not (empty? x))
                     x
                     (py input))))))

         (define choice
           (lambda (parsers)
             (fold-right or-else zero parsers)))

         ;; (define choice
         ;;   (lambda (parsers)
         ;;    (fold-left or-else (car parsers) (cdr parsers))))

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

         ;; === sequences ===

         (define and-then
           (lambda (px py)
             (monad-do (x <- px)
                       (y <- py)
                       (return (cons x y)))))

         ;; (define and-then
         ;;   (lambda (px py)
         ;;     (bind px (lambda (x)
         ;;                (bind py (lambda (y)
         ;;                           (return (cons x y))))))))

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