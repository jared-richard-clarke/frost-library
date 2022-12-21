(import (rnrs))

;; (parser list) -> '(a list) 

;; (item list) -> '(a list)

(define (item xs)
  (if (null? xs)
      (zero)
      (list (car xs) (cdr xs))))

;; (return a) -> m a

(define (return x)
  (lambda (xs)
    (list x xs)))

;; (bind (m a) (a -> (m b))) -> (m b)

(define (bind m f)
  (lambda (input)
    (let ([lst (m input)])
      ((f (car lst)) (cadr lst)))))

;; (zero list) -> '()

(define (zero . xs) '())

;; (satisfy (char -> bool)) -> parser

(define (satisfy pred)
  (bind item
        (lambda (x)
          (if (pred x)
              (return x)
              (zero)))))

(define (parse-char x)
  (satisfy (lambda (y) (char=? x y))))
