;; === UNDER HEAVY CONSTRUCTION ===

(library (data)
         (export context
                 state
                 CONSUMED
                 EMPTY
                 OK
                 ERROR)
         (import (rnrs))
         
         ;; Side-Note: In Haskell, Consumed and Reply are usually implemented as data constructors — functions
         ;; that return data types. Non-strict semantics — a.k.a. lazy evaluation — prevents these
         ;; constructors from immediately returning values the parser might later discard.

         (define CONSUMED 'Consumed)
         (define EMPTY    'Empty)
         (define OK       'Ok)
         (define ERROR    'Error)

         (define-record-type state (fields input line column))

         (define-record-type context
           (fields consumed  ;;  Consumed | Empty
                   reply     ;;  Ok | Error
                   output    ;;  Any
                   state))   ;; (list char) number number

         )
