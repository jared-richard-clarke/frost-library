;; === UNDER CONSTRUCTION ===

(import (rnrs)
        (frost combinators)
        (frost parsers)
        (frost parse))

;; === json parser ===

(define json-true  (text "true"))
(define json-false (text "false"))
(define json-null  (text "null"))
(define comma (character #\,))
(define colon (character #\:))
(define quote-mark (character #\"))

(define escape-character
  (choice quote-mark
          (character #\\)
          (character #\/)
          (character #\backspace)
          (character #\page)
          linefeed
          (character #\return)
          tab))

(define json (lambda (xs) (json-element xs)))

(define json-value
  (choice json-object
          json-array
          json-string
          json-number
          json-true
          json-false
          json-null))

(define json-object
  (between (character #\{)
           (choice json-members skip-spaces)
           (character #\})))

(define json-members (sep-by-1 json-member comma))

(define json-member
  (monad-do (key   <- (trim json-string))
            (col   <- colon)
            (value <- json-element)
            (return (list key ': value))))

(define json-array
  (between (character #\[)
           (choice json-elements skip-spaces)
           (character #\])))

(define json-elements (sep-by-1 json-element comma))

(define json-element (trim json-value))

(define json-string
  (between quote-mark
           json-characters
           quote-mark))

(define json-characters (many json-character))

(define json-character (none-of "\""))
