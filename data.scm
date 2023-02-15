;; === UNDER HEAVY CONSTRUCTION ===

(library (data)
  (export Parser-Consumer
	  CONSUMED-OK
	  CONSUMED-ERROR
	  EMPTY-OK
	  EMPTY-ERROR)
  (import (rnrs))
	 
  (define-enumeration Consumer-Enum (CONSUMED EMPTY OK ERROR) make-consumer)

  (define CONSUMED-OK    (make-consumer CONSUMED OK))
  (define CONSUMED-ERROR (make-consumer CONSUMED ERROR)) 

  (define EMPTY-OK    (make-consumer EMPTY OK))
  (define EMPTY-ERROR (make-consumer EMPTY ERROR))

  (define-record-type Position (fields line column))

  (define-record-type (Parser-Consumer consume consumer?)
    (fields type     ;; Consumed Ok | Consumed Error | Empty Ok | Empty Error
	    input    ;; (list char)
	    output)) ;; Any
)
