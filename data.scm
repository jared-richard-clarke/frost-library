;; === UNDER HEAVY CONSTRUCTION ===

(library (data)
         (export context
                 state
                 message
                 CONSUMED
                 EMPTY
                 OK
                 ERROR)
         (import (rnrs))
         
         (define CONSUMED 'Consumed)
         (define EMPTY    'Empty)
         (define OK       'Ok)
         (define ERROR    'Error)

         (define-record-type state
           (fields input    ;; string
                   line     ;; number
                   column)) ;; number

         (define-record-type message
           (fields unexpected ;;  string
                   expected)) ;; (list string)

         (define-record-type context
           (fields consumed  ;;  Consumed | Empty
                   reply     ;;  Ok | Error
                   output    ;;  Any
                   state))   ;; (list char) number number

         )
