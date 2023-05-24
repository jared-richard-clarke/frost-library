;; === UNDER HEAVY CONSTRUCTION ===

(library (data)
         (export context
                 state)
         (import (rnrs))

         (define-record-type state
           (fields input    ;; (list character)
                   line     ;; number
                   column)) ;; number

         (define-record-type context
           (fields reply    ;;  Consumed Ok | Consumed Error | Empty Ok | Empty Error
                   state    ;; (list character) number number
                   output)) ;;  Any

         )
