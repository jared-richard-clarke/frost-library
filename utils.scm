(library (utils)
         (export empty?
                 char-in?
                 symbol-in?
                 compose
                 assert-test)
         (import (rnrs))
         
         (define empty? null?)

         (define inside
           (lambda (fn)
             (lambda (x xs)
               (let loop ([x x] [xs xs])
                 (if (empty? xs)
                     #f
                     (if (fn x (car xs))
                         #t
                         (loop x (cdr xs))))))))

         (define char-in?   (inside char=?))
         
         (define symbol-in? (inside eq?))

         (define (compose . functions)
           (lambda (arg)
             (fold-right (lambda (function value)
                           (function value))
                         arg
                         functions)))

         (define-syntax assert-test
           (lambda (stx)
             (syntax-case stx ()
               [(_ compare x y)
                (syntax (let ([computed-x x]
                              [computed-y y])
                          (unless (compare computed-x computed-y)
                            (printf "Test failed:\nlhs: ~a -> ~a, rhs: ~a -> ~a\n"
                                    (quote x)
                                    x
                                    (quote y)
                                    y))))])))
         )
