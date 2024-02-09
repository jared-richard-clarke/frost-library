;; === UNDER CONSTRUCTION ===

(import (rnrs)
        (frost combinators)
        (frost parsers)
        (frost parse))

;; === json parser ===

(define OBJECT 'object)
(define ARRAY 'array)

(define comma      (character #\,))
(define colon      (character #\:))
(define quote-mark (character #\"))
(define exponent   (one-of "eE"))
(define keyword
  (lambda (txt value)
    (let ([parser (apply sequence (map character (string->list txt)))])
      (label txt (replace parser (return value))))))

;; json ::= element
(define json
  (lambda (input)
    (json-element input)))

;; value ::= object
;;         | array
;;         | string
;;         | number
;;         | "true"
;;         | "false"
;;         | "null"
(define json-value
  (lambda (input)
    ((choice json-object
             json-array
             json-string
             json-number
             json-true
             json-false
             json-null)
     input)))

;; object ::= "{" spaces "}"
;;          | "{" members "}"
(define json-object
  (lambda (input)
    ((label "object: {...}"
            (monad-do (ms <- (between (character #\{)
                                      (choice json-members skip-spaces)
                                      (character #\})))
                      (return (cons OBJECT ms))))
     input)))

;; members ::= member
;;           | member "," members
(define json-members
  (lambda (input)
    ((sep-by-1 json-member comma) input)))

;; member ::= spaces string spaces ":" element
(define json-member
  (lambda (input)
    ((monad-do (key   <- (trim json-string))
               (col   <- colon)
               (value <- json-element)
               (return (list key value)))
     input)))

;; array ::= "[" spaces "]"
;;         | "[" elements "]"
(define json-array
  (lambda (input)
    ((label "array: [...]"
            (monad-do (es <- (between (character #\[)
                                      (choice json-elements skip-spaces)
                                      (character #\])))
                      (return (cons ARRAY es))))
     input)))

;; elements ::= element
;;            | element "," elements
(define json-elements
  (lambda (input)
    ((sep-by-1 json-element comma) input)))

;; element ::= spaces value spaces
(define json-element
  (lambda (input)
    ((trim json-value) input)))

;; string ::= '"' characters '"'
(define json-string
  (lambda (input)
    ((label "string: \"...\""
            (between quote-mark
                     (fmap list->string json-characters)
                     quote-mark))
     input)))

;; characters ::= "" | character characters
(define json-characters
  (lambda (input)
    ((many json-character) input)))

;; character ::= '0020' . '10FFFF' - '"' - '\'
;;             | '\' escape
;;
;; escape ::= '"' | '\' | '/' | 'b' | 'f' | 'n' | 'r' | 't'
(define json-character
  (lambda (input)
    ((none-of "\"") input)))

;; number ::= integer fraction exponent
(define json-number
  (lambda (input)
    ((label "number"
            (monad-do (r <- real)
                      (e <- (either (replace exponent integer) 0))
                      (return (* r (expt 10.0 e)))))
     input)))

(define json-true  (keyword "true" 'true))
(define json-false (keyword "false" 'false))
(define json-null  (keyword "null" 'null))
