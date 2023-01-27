(import (rnrs)
        (parsers)
        (utils))

;; === Unit Tests ===
;;
;; Side Note: Cannot compare functions directly. Must compare their outputs.

;; test data
(define abc (string->list "abc"))

;; === Monad Laws ===
;;
;;            return a >>= f = f a <------------------------ Right argument to "bind" involves a binding operation.
;;              p >>= return = p <-------------------------- "return" is a right and left unit for "bind".
;; p >>= (\a -> (f a >>= g)) = (p >>= (\a -> f a)) >>= g <-- "bind" is associative.

;; return a >>= f = f a
;;              ^---^ Must be a monad-producing function.
(define test-a-lhs
  (bind (return '()) return))

(define test-a-rhs
  (return '()))

(assert-test equal? (test-a-lhs abc) (test-a-rhs abc))

;; p >>= return = p
;;       ^ Must be a monad-producing function.
(define test-b-lhs (bind item return))
(define test-b-rhs item)

(assert-test equal? (test-b-lhs abc) (test-b-rhs abc))

;; p >>= (\a -> (f a >>= g)) = (p >>= (\a -> f a)) >>= g
;;               ^-------^-------------------^---------^ Must be monad-producing functions.
(define test-c-lhs
  (bind item (lambda (a)
               (bind (return a) return))))

(define test-c-rhs
  (bind (bind item return) return))

(assert-test equal? (test-c-lhs abc) (test-c-rhs abc))
