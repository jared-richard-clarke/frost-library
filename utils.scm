(library (frost utils)
         (export empty?
                 identity
                 char-in?
                 symbol-in?
                 compose
                 repeat
                 digit->integer
                 fold-digits-by
                 assert)
         (import (rnrs base)
                 (rnrs lists)
                 (rnrs control))
         
         ;; Provides alias for "null?".
         ;; In Scheme, "null" describes an empty list.
         (define empty? null?)

         ;; Outputs a value unchanged.
         (define identity (lambda (x) x))

         ;; Inputs a predicate and outputs a function that
         ;; queries whether a value is in a list of values.
         (define member
           (lambda (test)
             (lambda (x xs)
               (let loop ([y x] [ys xs])
                 (cond
                   [(empty? ys) #f]
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

         ;; Converts numeric characters into their numeric equivalents.
         (define digit->integer
           (lambda (x)
             (- (char->integer x) (char->integer #\0))))

         ;; Given a radix returns a function that folds sequences of digits into their accumulative
         ;; numerical equivalents. Radix determines position.
         (define fold-digits-by
           (lambda (radix)
             (lambda (xs)
               (fold-left (lambda (sum x)
                            (+ (* radix sum) (digit->integer x)))
                          0
                          xs))))

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
