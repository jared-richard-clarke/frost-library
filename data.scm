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
           (fields input    ;; (list character)
                   line     ;; number
                   column)) ;; number

         ;; Represents either success or failure.
         (define-record-type result
           (fields success
                   failure))

         )
