;; === UNDER HEAVY CONSTRUCTION ===

(library (parsers)
         (export character
                 any-character
                 digit
                 letter
                 upper-case
                 lower-case
                 alpha-num
                 one-of
                 none-of
                 space
                 new-line
                 crlf
                 tab
                 punctuation
                 punctuation-ascii
                 digits
                 letters
                 spaces
                 trim-left
                 trim-right
                 trim
                 text
                 lexeme)
         (import (rnrs)
                 (combinators)
                 (utils))

         (define character
           (lambda (x)
             (satisfy (lambda (y) (char=? x y)))))

         (define any-character
           (satisfy (lambda (x) #t)))

         (define digit 
           (satisfy char-numeric?))

         (define letter
           (satisfy char-alphabetic?))

         (define upper-case
           (satisfy char-upper-case?))

         (define lower-case
           (satisfy char-lower-case?))

         (define alpha-num
           (or-else letter digit))

         (define one-of
           (lambda (txt)
             (let ([xs (string->list txt)])
               (satisfy (lambda (x) (char-in? x xs))))))

         (define none-of
           (lambda (txt)
             (let ([xs (string->list txt)])
               (satisfy (lambda (x) (not (char-in? x xs)))))))

         (define space 
           (satisfy char-whitespace?))
         
         (define new-line
           (character #\newline))
         
         (define crlf
           (right (character #\return) new-line))
         
         (define tab
           (character #\tab))
         
         ;; Finds all punctuation as defined by Unicode.
         (define punctuation
           (satisfy (lambda (x)
                      (let ([category    (char-general-category x)]
                            [categories '(Ps Pe Pi Pf Pd Pc Po)])
                        (symbol-in? category categories)))))

         ;; Finds all punctuation as defined by ASCII. Subsumed by Unicode.
         (define punctuation-ascii
           (satisfy (let ([ascii (string->list "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")])
                      (lambda (x) 
                        (char-in? x ascii)))))

         (define digits (many-1 digit))

         (define letters (many-1 letter))

         (define spaces (skip-many space))

         (define trim-left
           (lambda (px)
             (right spaces px)))

         (define trim-right
           (lambda (px)
             (left px spaces)))

         (define trim
           (lambda (px)
             (between spaces px spaces)))

         (define text
           (lambda (txt)
             (let ([characters (string->list txt)])
               (fmap list->string (sequence (map character characters))))))

         (define lexeme
           (lambda (px)
             (trim-right px)))
         
         )
