;; === UNDER CONSTRUCTION ===

(import (rnrs)
        (frost combinators)
        (frost parsers)
        (frost parse))

;; === json parser ===

(define object-label 'object)
(define array-label  'array)

(define comma      (character #\,))
(define colon      (character #\:))
(define quote-mark (character #\"))
(define exponent   (one-of "eE"))

;; json ::= element
(define json
  (lambda (state)
    (json-element state)))

;; value ::= object
;;         | array
;;         | string
;;         | number
;;         | "true"
;;         | "false"
;;         | "null"
(define json-value
  (lambda (state)
    ((choice json-object
             json-array
             json-string
             json-number
             json-true
             json-false
             json-null)
     state)))

;; object ::= "{" spaces "}"
;;          | "{" members "}"
(define json-object
  (lambda (state)
    ((label "object: {...}"
            (monad-do (ms <- (between (character #\{)
                                      (choice json-members skip-spaces)
                                      (character #\})))
                      (return (cons object-label ms))))
     state)))

;; members ::= member
;;           | member "," members
(define json-members
  (lambda (state)
    ((sep-by-1 json-member comma) state)))

;; member ::= spaces string spaces ":" element
(define json-member
  (lambda (state)
    ((monad-do (key   <- (trim json-string))
               (col   <- colon)
               (value <- json-element)
               (return (list key value)))
     state)))

;; array ::= "[" spaces "]"
;;         | "[" elements "]"
(define json-array
  (lambda (state)
    ((label "array: [...]"
            (monad-do (es <- (between (character #\[)
                                      (choice json-elements skip-spaces)
                                      (character #\])))
                      (return (cons array-label es))))
     state)))

;; elements ::= element
;;            | element "," elements
(define json-elements
  (lambda (state)
    ((sep-by-1 json-element comma) state)))

;; element ::= spaces value spaces
(define json-element
  (lambda (state)
    ((trim json-value) state)))

;; string ::= '"' characters '"'
(define json-string
  (lambda (state)
    ((label "string: \"...\""
            (between quote-mark
                     (fmap list->string json-characters)
                     quote-mark))
     state)))

;; characters ::= "" | character characters
(define json-characters
  (lambda (state)
    ((many json-character) state)))

;; character ::= '0020' . '10FFFF' - '"' - '\'
;;             | '\' escape
;;
;; escape ::= '"' | '\' | '/' | 'b' | 'f' | 'n' | 'r' | 't'
(define json-character
  (lambda (state)
    ((none-of "\"") state)))

;; number ::= integer fraction exponent
(define json-number
  (lambda (state)
    ((label "number"
            (monad-do (r <- real)
                      (e <- (either (replace exponent integer) 0))
                      (return (* r (expt 10.0 e)))))
     state)))

(define json-true  (text "true"))
(define json-false (text "false"))
(define json-null  (text "null"))
