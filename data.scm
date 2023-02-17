;; === UNDER HEAVY CONSTRUCTION ===

(library (data)
         (export context
                 CONSUMED
                 EMPTY
                 OK
                 ERROR)
         (import (rnrs))

         (define CONSUMED 'Consumed)
         (define EMPTY    'Empty)
         (define OK       'Ok)
         (define ERROR    'Error)

         (define-record-type position (fields line column))

         (define-record-type context
           (fields consumed  ;; Consumed | Empty
                   reply     ;; Ok | Error
                   output    ;; Any
                   input))   ;; (list char)

         )
