(import (rnrs)
        (frost))

;; === JSON parser ===
;; grammar: https://www.json.org/json-en.html

;; In Scheme, forward references are allowed only within the bodies of function
;; definitions - a.k.a lambda abstractions. Wrapping each parser within an extra
;; lambda abstraction both allows the construction of mutually-defined grammars
;; and simplifies incremental parser construction.

(define OBJECT 'Object)
(define ARRAY 'Array)

(define comma      (character #\,))
(define colon      (character #\:))
(define quote-mark (character #\"))
(define zero       (character #\0))
(define exponent   (one-of "eE"))

(define keyword
  (lambda (txt value)
    (let ([parser (apply sequence (map character (string->list txt)))])
      (label txt (replace parser (return value))))))

;; JSON <- Element
(define json
  (lambda (input)
    (json-element input)))

;; Value <- Object
;;        / Array
;;        / String
;;        / Number
;;        / True
;;        / False
;;        / Null
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

;; Object <- "{" ( Members / Spaces? ) "}"
(define json-object
  (lambda (input)
    ((label "object: {...}"
            (monad-do (ms <- (between (character #\{)
                                      (choice json-members skip-spaces)
                                      (character #\})))
                      (return (cons OBJECT ms))))
     input)))

;; Members <- Member ( "," Member )*
(define json-members
  (lambda (input)
    ((sep-by-1 json-member comma) input)))

;; Member ::= Spaces? String Spaces? ":" Element
(define json-member
  (lambda (input)
    ((monad-do (key <- (trim json-string))
               (col <- colon)
               (val <- json-element)
               (return (list key val)))
     input)))

;; Array <- "[" Elements / Spaces? "]"
(define json-array
  (lambda (input)
    ((label "array: [...]"
            (monad-do (es <- (between (character #\[)
                                      (choice json-elements skip-spaces)
                                      (character #\])))
                      (return (cons ARRAY es))))
     input)))

;; Elements <- Element ( "," Element )*
(define json-elements
  (lambda (input)
    ((sep-by-1 json-element comma) input)))

;; Element <- Spaces? Value Spaces?
(define json-element
  (lambda (input)
    ((trim json-value) input)))

;; String <- '"' Characters '"'
(define json-string
  (lambda (input)
    ((label "string: \"...\""
            (between quote-mark
                     (fmap list->string json-characters)
                     quote-mark))
     input)))

;; Characters <- Character*
(define json-characters
  (lambda (input)
    ((many json-character) input)))

;; Character <- !'"'.
(define json-character
  (lambda (input)
    ((none-of "\"") input)))

;; Number   <- Real Exponent?
;; Real     <- ( "+" / "-" )? ( "0" / [1-9] [0-9]* ( "." [0-9]+ )? )
;; Exponent <- ( "e" / "E" ) ( "+" / "-" )? [0-9]+
(define json-number
  (lambda (input)
    ((label "number"
            (monad-do (r <- real)
                      (e <- (either (replace exponent integer) 0))
                      (return (* r (expt 10.0 e)))))
     input)))

;; True  <- "true"
(define json-true  (keyword "true" 'true))
;; False <- "false"
(define json-false (keyword "false" 'false))
;; Null  <- "null"
(define json-null  (keyword "null" 'null))
