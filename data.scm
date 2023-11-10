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
                 state-update-char
                 result)
         (import (rnrs base)
                 (rnrs records syntactic))

         ;; enumeration:  Consumed Ok | Consumed Error | Empty Ok | Empty Error
         ;; === elements ===
         (define CONSUMED 'CONSUMED)
         (define EMPTY    'EMPTY)
         (define OK       'OK)
         (define ERROR    'ERROR)
         ;; === combinations ===
         (define CONSUMED-OK    (list CONSUMED OK))
         (define CONSUMED-ERROR (list CONSUMED ERROR))
         (define EMPTY-OK       (list EMPTY OK))
         (define EMPTY-ERROR    (list EMPTY ERROR))

         ;; === constants ===
         (define NONE '())

         ;; === data types ===
         ;; Tracks input as it is consumed by parser.
         (define-record-type state
           (fields input    ;; (list any)
                   line     ;; number
                   column   ;; number
                   update)) ;; function <- Updates the state. Must be defined according to input.

         ;; Updates the state for an input that is a list of characters.
         (define state-update-char
           (lambda (char chars line column)
             ;; Although #\linefeed and #\newline are synonymous
             ;; older Schemes recognize only #\newline
             (if (or (char=? char #\linefeed) (char=? char #\newline))
                 (make-state chars (+ line 1) 0 state-update-char)
                 (make-state chars line (+ column 1) state-update-char))))


         ;; Represents either success or failure.
         (define-record-type result
           (fields success
                   failure))

         )
