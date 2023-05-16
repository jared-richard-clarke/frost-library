;; === UNDER HEAVY CONSTRUCTION ===

(library (data)
         (export context
                 state)
         (import (rnrs))

         (define-record-type state
           (fields input    ;; string
                   line     ;; number
                   column)) ;; number

         (define-record-type context
           (fields reply    ;;  Consumed Ok | Consumed Error | Empty Ok | Empty Error
                   state    ;; (list char) number number
                   output)) ;;  Any

         )
