(library (frost parsers)
         (export one-of
                 none-of
                 character
                 singleton
                 any
                 digit
                 denary-digit
                 binary-digit
                 digits
                 whole
                 integer
                 real
                 rational
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

         ;; Creates a parser for a single character. If successful,
         ;; returns character.
         (define character
           (lambda (x)
             (label (string x) (satisfy (lambda (y) (char=? x y))))))

         ;; Creates a parser for a single character. If successful,
         ;; returns character wrapped in a list.
         (define singleton
           (lambda (x)
             (monad-do (ch <- (satisfy (lambda (y) (char=? x y))))
                       (return (list x)))))

         ;; Parses any unicode character, including whitespace.
         (define any
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

         ;; Helper function parses an optional sign: +|-
         (define sign
           (optional (choice (singleton #\-)
                             (singleton #\+))))

         ;; Helper function ensures denary whole numbers don't start with zero: !0 digits
         (define integral
           (choice (singleton #\0) digits))

         ;; Helper function parses an optional fraction component: .[0-9]
         (define fractional
           (optional (monad-do (p  <- (singleton #\.))
                               (ds <- digits)
                               (return (append p ds)))))

         ;; Creates a parser that converts a sequence of digits into their numerical equivalent
         ;; using the given radix and parser.
         (define base
           (lambda (radix parser)
             (monad-do (xs <- parser)
                       (let ([number (string->number (list->string xs) radix)])
                         (if (not (number? number))
                             fail
                             (return number))))))

         ;; Parses and returns a whole number in base 10. 
         ;; [0, 1, 2 ...]
         (define whole (label "whole number" (base 10 integral)))

         ;; Parses and returns an integer in base 10.
         ;; [... -2, -1, 0, 1, 2 ...]
         (define integer
           (label "integer"
                  (base 10 (monad-do (s <- sign)
                                     (x <- integral)
                                     (return (append s x))))))

         ;; Parses and returns a real number in base 10.
         ;; [... -4 ... 0 ... 0.25 ... 7.5 ... 11 ...]
         (define real
           (label "real number"
                  (base 10 (monad-do (s <- sign)
                                     (x <- integral)
                                     (y <- fractional)
                                     (return (append s x y))))))

         ;; Parses and returns a rational number in base 10.
         ;; [...-1/2 ... 3/4 ... 7/11 ...]
         (define rational
           (label "rational number"
                  (base 10 (monad-do (s <- sign)
                                     (x <- integral)
                                     (y <- (singleton #\/))
                                     (z <- integral)
                                     (return (append s x y z))))))

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
           (choice letter digit))

         ;; Parses any character that satisfies the predicate "char-whitespace?".
         (define space 
           (label "whitespace" (satisfy char-whitespace?)))

         ;; Parses zero or more white spaces.
         (define spaces (many space))

         ;; Parses zero or more white spaces and ignores the result.
         (define skip-spaces (skip-many space))

         ;; Parses a linefeed control character.
         (define linefeed
           (choice (character #\linefeed) (character #\newline)))
         
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
