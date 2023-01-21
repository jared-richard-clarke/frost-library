;; Highly-experimental macro for the Haskell "do" syntax

(define-syntax do
  (lambda (stx)
    (syntax-case stx (<-)
      [(_ pn)
       (syntax pn)]
      [(_ (x <- px) pn ...)
       (syntax (bind px (lambda (x) 
                          (do pn ...))))])))
