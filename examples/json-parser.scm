;; === UNDER CONSTRUCTION ===

(import (rnrs)
        (frost combinators)
        (frost parsers)
        (frost parse))

;; === json parser ===

(define comma      (character #\,))
(define colon      (character #\:))
(define quote-mark (character #\"))
(define exponent   (one-of "eE"))

;; json ::= element
(define json json-element)

;; value ::= object
;;         | array
;;         | string
;;         | number
;;         | "true"
;;         | "false"
;;         | "null"
(define json-value
  (choice json-object
          json-array
          json-string
          json-number
          json-true
          json-false
          json-null))

;; object ::= "{" spaces "}"
;;          | "{" members "}"
(define json-object
  (between (character #\{)
           (choice json-members skip-spaces)
           (character #\})))

;; members ::= member
;;           | member "," members
(define json-members
  (sep-by-1 json-member comma))

;; member ::= spaces string spaces ":" element
(define json-member
  (monad-do (key   <- (trim json-string))
            (col   <- colon)
            (value <- json-element)
            (return (list key ': value))))

;; array ::= "[" spaces "]"
;;         | "[" elements "]"
(define json-array
  (between (character #\[)
           (choice json-elements skip-spaces)
           (character #\])))

;; elements ::= element
;;            | element "," elements
(define json-elements
  (sep-by-1 json-element comma))

;; element ::= spaces value spaces
(define json-element
  (trim json-value))

;; string ::= '"' characters '"'
(define json-string
  (between quote-mark
           json-characters
           quote-mark))

;; characters ::= "" | character characters
(define json-characters
  (many json-character))

;; character ::= '0020' . '10FFFF' - '"' - '\'
;;             | '\' escape
;;
;; escape ::= '"' | '\' | '/' | 'b' | 'f' | 'n' | 'r' | 't'
(define json-character
  (none-of "\""))

;; number ::= integer fraction exponent
(define json-number
  (monad-do (r <- real)
            (e <- (either (replace exponent integer) 0))
            (return (* r (expt 10 e)))))

(define json-true  (text "true"))
(define json-false (text "false"))
(define json-null  (text "null"))
