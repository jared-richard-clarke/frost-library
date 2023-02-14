;; === UNDER HEAVY CONSTRUCTION ===

(library (data)
  (export Parser-Consumer)
  (import (rnrs))

  (define-enumeration Consumer-Enum (CONSUMED EMPTY OK ERROR) make-consumer)

  ;; Consumer
  (define CO-EM (make-consumer CONSUMED EMPTY))
  (define CO    (make-consumer CONSUMED))
  (define EM    (make-consumer EMPTY))

  ;; Reply
  (define OK-ER (make-consumer OK ERROR))
  (define OK    (make-consumer OK))
  (define ER    (make-consumer ERROR))


  (define-record-type Parser-Consumer
    (fields (mutable consumer) ;; Consumed | Empty
            (mutable reply)    ;; Ok | Error
            (mutable output)   ;; Any
            (mutable input)))  ;; (list char)
  )
