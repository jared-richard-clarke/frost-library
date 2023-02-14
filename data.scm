;; === UNDER HEAVY CONSTRUCTION ===

(library (data)
  (export Parser-Consumer)
  (import (rnrs))

  (define-enumeration Consumer-Enum (CONSUMED EMPTY OK ERROR) make-consumer)

  (define CONSUMED-OK    (make-consumer CONSUMED OK))
  (define CONSUMED-ERROR (make-consumer CONSUMED ERROR))

  (define EMPTY-OK    (make-consumer EMPTY OK))
  (define EMPTY-ERROR (make-consumer EMPTY ERROR))


  (define-record-type Parser-Consumer
    (fields (mutable consumer) ;; Consumed Ok | Consumed Error | Empty Ok | Empty Error
            (mutable output)   ;; Any
            (mutable input)))  ;; (list char)
  )
