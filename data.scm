;; === UNDER HEAVY CONSTRUCTION ===

(library (data)
         (export context
                 state)
         (import (rnrs base)
                 (rnrs records syntactic))

         ;; Tracks input as it is consumed by parser.
         (define-record-type state
           (fields input    ;; (list character)
                   line     ;; number
                   column)) ;; number

         ;; Tracks parser consumption, state, and output.
         (define-record-type context
           (fields reply    ;;  Consumed Ok | Consumed Error | Empty Ok | Empty Error
                   state    ;; (list character) number number
                   output)) ;;  Any

         )
