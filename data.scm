;; === UNDER HEAVY CONSTRUCTION ===

(library (data)
  (export context
	  CONSUMED
	  EMPTY
	  OK
	  ERROR
	  UNKNOWN)
  (import (rnrs))
	 
  (define CONSUMED 'Consumed)
  (define EMPTY 'Empty)
  (define OK 'Ok)
  (define ERROR 'Error)
  (define UNKNOWN 'UNKNOWN)

  (define-record-type position (fields line column))

  (define-record-type context
    (fields consumed  ;; Consumed | Empty
	    reply     ;; Ok | Error | Unknown
	    output    ;; Any
	    input))   ;; (list char)
)
