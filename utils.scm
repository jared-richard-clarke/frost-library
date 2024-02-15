(library (frost utils)
         (export identity
                 string->vector
                 char-in?
                 symbol-in?
                 compose
                 repeat
                 assert)
         (import (rnrs base)
                 (rnrs lists)
                 (rnrs control))

         ;; Outputs a value unchanged.
         (define identity (lambda (x) x))

         ;; Transforms a string into a vector.
         (define string->vector
           (lambda (x)
             (list->vector (string->list x))))

         ;; Inputs a predicate and outputs a function that
         ;; queries whether a value is in a list of values.
         (define member
           (lambda (test)
             (lambda (x xs)
               (let loop ([y x] [ys xs])
                 (cond
                   [(null? ys) #f]
                   [(test y (car ys)) #t]
                   [else (loop y (cdr ys))])))))

         ;; Queries whether a character is in a list of characters.
         (define char-in?   (member char=?))

         ;; Queries whether a symbol is in a list of symbols.
         (define symbol-in? (member eq?))

         ;; Composes a series of functions into a single function expression.
         ;; Functions are applied right to left.
         (define (compose . functions)
           (lambda (arg)
             (fold-right (lambda (function value)
                           (function value))
                         arg
                         functions)))

         ;; Builds a repeated list of a given value.
         (define repeat
           (lambda (number value)
             (if (<= number 0)
                 '()
                 (cons value (repeat (- number 1) value)))))

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
                         (display "lhs: ") (write (quote x)) (display " -> ") (write computed-x) (display ",")
                         (newline)
                         (display "rhs: ") (write (quote y)) (display " -> ") (write computed-y)
                         (newline))))]))

         )
