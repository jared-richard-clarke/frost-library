(library (utils)
         (export empty?
                 compose
                 assert-test)
         (import (rnrs))
         
         (define empty? null?)

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
                            (printf "Test failed:\nrhs: ~a -> ~a, lhs: ~a -> ~a\n"
                                    (quote x)
                                    x
                                    (quote y)
                                    y))))])))
         )
