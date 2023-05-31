;; === UNDER HEAVY CONSTRUCTION ===

(library (parsers)
         (export character
                 any-character
                 digit
                 digits
                 letter
                 letters
                 upper-case
                 lower-case
                 alpha-num
                 one-of
                 none-of
                 space
                 spaces
                 skip-spaces
                 linefeed
                 crlf
                 tab
                 punctuation
                 punctuation-ascii
                 trim-left
                 trim-right
                 trim
                 text)
         (import (rnrs)
                 (combinators)
                 (utils))

         ;; === parsers ===

         ;; Creates a parser for a single character.
         (define character
           (lambda (x)
             (satisfy (lambda (y) (char=? x y)))))

         ;; Parses any unicode character, including whitespace.
         (define any-character
           (satisfy (lambda (x) (char? x))))

         ;; Parses any digit that satisfies the predicate "char-numeric?".
         (define digit 
           (satisfy char-numeric?))

         ;; Parses a sequence of one or more digits.
         (define digits (many-1 digit))

         ;; Parses any letter that satisfies the predicate "char-alphabetic?". 
         (define letter
           (satisfy char-alphabetic?))

         ;; Parses a sequence of one or more letters.
         (define letters (many-1 letter))

         ;; Parses any character that satisfies the predicate "char-upper-case?".
         (define upper-case
           (satisfy char-upper-case?))

         ;; Parses any character that satisfies the predicate "char-lower-case?".
         (define lower-case
           (satisfy char-lower-case?))

         ;; Parses any letter or digit.
         (define alpha-num
           (or-else letter digit))

         ;; Creates a parser for any character defined within its string argument.
         (define one-of
           (lambda (txt)
             (let ([xs (string->list txt)])
               (satisfy (lambda (x) (char-in? x xs))))))

         ;; Creates a parser for any character not defined within its string argument.
         (define none-of
           (lambda (txt)
             (let ([xs (string->list txt)])
               (satisfy (lambda (x) (not (char-in? x xs)))))))

         ;; Parses any character that satisfies the predicate "char-whitespace?".
         (define space 
           (satisfy char-whitespace?))

         ;; Parses zero or more white spaces.
         (define spaces (many space))

         ;; Parses zero or more white spaces and ignores the result.
         (define skip-spaces (skip-many space))

         ;; Parses a linefeed control character.
         (define linefeed 
           (or-else (character #\linefeed) (character #\newline)))
         
         ;; Parses a carriage return and linefeed pair, outputting only the linefeed.
         (define crlf
           (right (character #\return) linefeed))
         
         (define tab (character #\tab))
         
         ;; Parses any punctuation as defined by Unicode.
         (define punctuation
           (satisfy (lambda (x)
                      (let ([category    (char-general-category x)]
                            [categories '(Po Ps Pe Pi Pf Pd Pc)])
                        (symbol-in? category categories)))))

         ;; Parses any punctuation as defined by ASCII. Subsumed by Unicode.
         (define punctuation-ascii
           (satisfy (let ([ascii (string->list "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")])
                      (lambda (x) 
                        (char-in? x ascii)))))

         ;; Creates a parser that removes all whitespace to the left the input parsed by "px".
         (define trim-left
           (lambda (px)
             (right skip-spaces px)))

         ;; Creates a parser that removes all whitespace to the right of the input parsed by "px".
         (define trim-right
           (lambda (px)
             (left px skip-spaces)))

         ;; Creates a parser that removes all whitespace to the left and right of the input parsed by "px".
         (define trim
           (lambda (px)
             (between skip-spaces px skip-spaces)))

         ;; Creates a parser of the provided string.
         (define text
           (lambda (txt)
             (let ([parser (sequence (map character (string->list txt)))])
               (fmap list->string parser))))

         )
