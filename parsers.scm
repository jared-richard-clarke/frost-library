(library (frost parsers)
         (export one-of
                 none-of
                 character
                 any-character
                 digit
                 denary-digit
                 binary-digit
                 digits
                 whole
                 integer
                 real
                 letter
                 letters
                 upper-case
                 lower-case
                 alpha-num
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
                 chunk
                 text)
         (import (rnrs base)
                 (rnrs unicode)
                 (rnrs records syntactic)
                 (frost combinators)
                 (frost utils))

         ;; === parsers ===
         
         ;; Creates a parser for any character defined within its string argument.
         (define one-of
           (lambda (text)
             (let ([xs (string->list text)])
               (label (string-append "one of " text)
                      (satisfy (lambda (x) (char-in? x xs)))))))

         ;; Creates a parser for any character not defined within its string argument.
         (define none-of
           (lambda (text)
             (let ([xs (string->list text)])
               (label (string-append "none of " text)
                      (satisfy (lambda (x) (not (char-in? x xs))))))))

         ;; Creates a parser for a single character.
         (define character
           (lambda (x)
             (label x (satisfy (lambda (y) (char=? x y))))))

         ;; Parses any unicode character, including whitespace.
         (define any-character
           (label "any character" (satisfy (lambda (x) (char? x)))))

         ;; Parses any digit that satisfies the predicate "char-numeric?".
         (define digit 
           (label "digit" (satisfy char-numeric?)))

         ;; Parses any denary digit: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9].
         (define denary-digit
           (label "denary digit" (one-of "0123456789")))

         ;; Parses any binary digit: [0, 1].
         (define binary-digit
           (label "binary digit" (one-of "01")))

         ;; Parses a sequence of one or more digits.
         (define digits
           (label "digits" (many-1 digit)))

         ;; Parses an optional sign and returns its functional equivalent.
         (define sign (either (or-else (replace (character #\-) (return -))
                                       (replace (character #\+) (return +)))
                              identity))

         ;; Creates a parser that converts a sequence of digits into their numerical equivalent using the given radix and parser.
         (define base
           (lambda (radix parser)
             (monad-do (xs <- parser)
                       (let ([number ((fold-digits-by radix) xs)])
                         (return number)))))

         ;; Creates a parser that converts a sequence of digits into their fractional numerical equivalent using the given radix and parser.
         (define fractional
           (lambda (radix parser)
             (monad-do (xs <- parser)
                       (let ([power  (expt radix (length xs))]
                             [number ((fold-digits-by radix) xs)])
                         (return (/ number power))))))

         ;; Parses a sequence of digits and returns a whole number in base 10.
         ;; [0, 1, 2 ...]
         (define whole (label "whole number" (base 10 digits)))

         ;; Parses an optional sign followed by a sequence of digits and returns an integer in base 10.
         ;; [... -2, -1, 0, 1, 2 ...]
         (define integer
           (label "integer"
                  (monad-do (f <- sign)
                            (x <- whole)
                            (return (f x)))))

         ;; Parses a sequence of digits and returns a fraction in base 10.
         ;; [0 ... 0.5 ... 1]
         (define decimal (fractional 10.0 digits)) ;; <- 10.0 ensures number is converted into floating point.

         ;; Parses an optional sign and a sequence of digits followed by an optional decimal point and a further sequence of digits.
         ;; Returns a real number in base 10.
         ;; [... -4 ... 0 ... 0.25 ... 7.5 ... 11 ...]
         (define real
           (label "real number"
                  (monad-do (f <- sign)
                            (x <- whole)
                            (y <- (either (replace (character #\.) decimal) 0))
                            (return (f (+ x y))))))

         ;; Parses any letter that satisfies the predicate "char-alphabetic?". 
         (define letter
           (label "letter" (satisfy char-alphabetic?)))

         ;; Parses a sequence of one or more letters.
         (define letters
           (label "letters" (many-1 letter)))

         ;; Parses any character that satisfies the predicate "char-upper-case?".
         (define upper-case
           (label "uppercase letter" (satisfy char-upper-case?)))

         ;; Parses any character that satisfies the predicate "char-lower-case?".
         (define lower-case
           (label "lowercase letter" (satisfy char-lower-case?)))

         ;; Parses any letter or digit.
         (define alpha-num
           (or-else letter digit))

         ;; Parses any character that satisfies the predicate "char-whitespace?".
         (define space 
           (label "whitespace" (satisfy char-whitespace?)))

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
           (label "punctuation"
                  (satisfy (lambda (x)
                             (let ([category    (char-general-category x)]
                                   [categories '(Po Ps Pe Pi Pf Pd Pc)])
                               (symbol-in? category categories))))))

         ;; Parses any punctuation as defined by ASCII. Subsumed by Unicode.
         (define punctuation-ascii
           (label "ascii punctuation"
                  (one-of "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")))

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
         
         ;; Parses chunk of consecutive non-whitespace characters.
         (define chunk
           (label "text chunk"
                  (many-1 (satisfy (lambda (x) (not (char-whitespace? x)))))))
         
         ;; Creates a parser of the provided string.
         (define text
           (lambda (txt)
             (let ([parser (apply sequence (map character (string->list txt)))])
               (label txt (fmap list->string parser)))))

         )
