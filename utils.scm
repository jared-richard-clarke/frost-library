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
               [(_ compare expression value)
                (syntax (let ([computed-expr expression]) ;; <- prevents redundant computation
                          (unless (compare computed-expr value)
                            (printf "Test: ~a\nExpect: ~a, Got: ~a\n"
                                    (quote expression) ;; <---- returns expression prior to evaluation
                                    value
                                    computed-expr))))])))
         )
