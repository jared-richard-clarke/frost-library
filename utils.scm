(library (utils)
         (export empty?
                 identity
                 char-in?
                 symbol-in?
                 compose
                 repeat
                 assert-test)
         (import (rnrs))
         
         (define empty? null?)
         
         (define identity (lambda (x) x))

         (define element
           (lambda (test)
             (lambda (x xs)
               (let loop ([x x] [xs xs])
                 (cond
                   [(empty? xs) #f]
                   [(test x (car xs)) #t]
                   [else (loop x (cdr xs))])))))

         (define char-in?   (element char=?))        
         (define symbol-in? (element eq?))

         (define (compose . functions)
           (lambda (arg)
             (fold-right (lambda (function value)
                           (function value))
                         arg
                         functions)))

         (define repeat
           (lambda (number value)
             (let loop ([n number]
                        [v value]
                        [r '()])
               (if (<= n 0)
                   r
                   (loop (- n 1) v (cons v r))))))
         
         (define-syntax assert-test
           (syntax-rules ()
             [(_ compare x y)
              (let ([computed-x x]
                    [computed-y y])
                (unless (compare computed-x computed-y)
                  (printf "Test failed:\nlhs: ~a -> ~a, rhs: ~a -> ~a\n"
                          (quote x)
                          computed-x
                          (quote y)
                          computed-y)))]))
         )
