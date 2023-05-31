;; === UNDER HEAVY CONSTRUCTION ===

(library (utils)
         (export empty?
                 identity
                 char-in?
                 symbol-in?
                 compose
                 repeat
                 assert)
         (import (rnrs base)
                 (rnrs lists))
         
         ;; Provides alias for "null?".
         ;; In Scheme, "null" describes an empty list.
         (define empty? null?)

         ;; Outputs a value unchanged.
         (define identity (lambda (x) x))

         ;; Inputs a predicate and outputs a function that
         ;; queries whether a value is in a list of values.
         (define element
           (lambda (test)
             (lambda (x xs)
               (let loop ([x x] [xs xs])
                 (cond
                   [(empty? xs) #f]
                   [(test x (car xs)) #t]
                   [else (loop x (cdr xs))])))))

         ;; Queries whether a character is in a list of characters.
         (define char-in?   (element char=?))

         ;; Queries whether a symbol is in a list of symbols.
         (define symbol-in? (element eq?))

         ;; Composes a series of functions into a single function expression.
         ;; Functions are applied right to left.
         (define (compose . functions)
           (lambda (arg)
             (fold-right (lambda (function value)
                           (function value))
                         arg
                         functions)))

         ;; Builds a list of a repeated value.
         (define repeat
           (lambda (number value)
             (let loop ([n number]
                        [v value]
                        [r '()])
               (if (<= n 0)
                   r
                   (loop (- n 1) v (cons v r))))))

         ;; If the left-hand-expression does not satisfy the predicate comparing it to the right-hand-expression,
         ;; "assert" prints the failed test to the current-output port.
         (define-syntax assert
           (syntax-rules ()
             [(_ compare x y)
              (let ([computed-x x]
                    [computed-y y])
                (unless (compare computed-x computed-y)
                  (begin (display "Test failed:")
                         (newline)
                         (display "lhs: ") (write (quote x)) (display " -> ") (write computed-x) (display ", ")
                         (display "rhs: ") (write (quote y)) (display " -> ") (write computed-y)
                         (newline))))]))
         
         )
