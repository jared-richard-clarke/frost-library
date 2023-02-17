;; === UNDER HEAVY CONSTRUCTION ===

(library (parsers)
         (export character
                 digit
                 letter
                 upper-case
                 lower-case
                 alpha-num
                 space
                 punctuation
                 punctuation-ascii
                 digits
                 letters
                 spaces
                 trim-left
                 trim-right
                 trim
                 any-of
                 text)
         (import (rnrs)
                 (combinators)
                 (utils))

         (define character
           (define character
             (lambda (x)
               (satisfy (lambda (y) (char=? x y)))))

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

           (define space 
             (satisfy char-whitespace?))
         
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

           (define spaces (many space))

           (define trim-left
             (lambda (px)
               (right (skip-many space) px)))

           (define trim-right
             (lambda (px)
               (left px (skip-many space))))

           (define trim
             (lambda (px)
               (let ([skip (skip-many space)])
                 (between skip px skip))))

           (define any-of
             (lambda (characters)
               (choice (map character characters))))

           (define text
             (lambda (str)
               (fmap list->string (sequence (map character (string->list str))))))
         
           )
