(library (frost data)
         (export CONSUMED
                 EMPTY
                 OK
                 ERROR
                 CONSUMED-OK
                 CONSUMED-ERROR
                 EMPTY-OK
                 EMPTY-ERROR
                 NONE
                 state
                 result
                 format-error)
         (import (rnrs base)
                 (rnrs records syntactic)
                 (frost utils))

         ;; enumeration:  Consumed Ok | Consumed Error | Empty Ok | Empty Error
         ;; === elements ===
         (define CONSUMED 'CONSUMED)
         (define EMPTY    'EMPTY)
         (define OK       'OK)
         (define ERROR    'ERROR)
         ;; === combinations ===
         (define CONSUMED-OK    (cons CONSUMED OK))
         (define CONSUMED-ERROR (cons CONSUMED ERROR))
         (define EMPTY-OK       (cons EMPTY OK))
         (define EMPTY-ERROR    (cons EMPTY ERROR))

         ;; === constants ===
         (define NONE '())

         ;; === data types ===
         ;; Tracks input as it is consumed by parser.
         (define-record-type state
           (fields input    ;; (vector char)
                   length   ;; number
                   offset   ;; number
                   line     ;; number
                   column)) ;; number

         ;; A tagged value representing either success or failure.
         (define-record-type result
           (fields flag    ;; OK | ERROR
                   value)) ;; any

         ;; === error formatting ===
         (define EMPTY-STRING "")
         (define COMMA        ", ")
         (define PERIOD       ". ")
         (define OR           " | ")
         (define NL           "\n")
         (define LINE         "Line: ")
         (define COLUMN       "Column: ")
         (define EOL          "<EOL>")
         (define EXPECTED     "Expected: ")
         (define GOT          "Got: ")
         (define GOT-EOL      (string-append GOT EOL))
         (define GOT-EMPTY    (string-append GOT "empty string"))

         (define format-error
           (lambda (state want)
             (let ([line   (string-append LINE   (number->string (state-line state)   10))]
                   [column (string-append COLUMN (number->string (state-column state) 10))]
                   [got  (let* ([input  (state-input state)]
                                [offset (state-offset state)]
                                [length (vector-length input)])   
                           (cond
                             [(< length 1) GOT-EMPTY]
                             [(<= length offset) GOT-EOL]
                             [else (string-append GOT (string (vector-ref input offset)))]))]
                   [expected (cond
                               [(null? want) EMPTY-STRING]
                               [(= (length want) 1) (string-append EXPECTED (car want))]
                               [else (apply string-append (cons EXPECTED
                                                                (cons (car want)
                                                                      (fold-right (lambda (x xs)
                                                                                    (cons OR (cons x xs)))
                                                                                  '()
                                                                                  (cdr want)))))])])
               (string-append line COMMA column PERIOD expected COMMA got))))

         )
