;; === UNDER HEAVY CONSTRUCTION ===

(library (data)
         (export context
                 CONSUMED
                 SAVED
                 OK
                 ERROR)
         (import (rnrs))

         (define CONSUMED 'Consumed)
         (define SAVED    'Saved)
         (define OK       'Ok)
         (define ERROR    'Error)

         (define-record-type position (fields line column))
         (define-record-type message  (fields text position))
         (define-record-type state    (fields text position))

         (define-record-type context
           (fields consumed  ;; Consumed | Saved
                   reply     ;; Ok | Error
                   output    ;; Any
                   input))   ;; (list char)

         )
