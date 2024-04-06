#lang racket

(provide (contract-out
          [make-vector-view (-> (or/c vector? vector-view?) natural-number/c vector-view?)]
          [vector-view-ref  (-> vector-view? natural-number/c any/c)]
          [vector-view-set! (-> vector-view? natural-number/c any/c any/c)])
         (struct-out vector-view))

(struct vector-view [vec offset])

(define (make-vector-view vec offset)
  (cond [(vector? vec)
         (vector-view vec offset)]
        [(vector-view? vec)
         (struct-copy vector-view vec
                      [offset (+ offset (vector-view-offset vec))])]
        [else
         (raise-type-error 'make-vector-view "expected view to have type vector or vector-view")]))

(define (vector-view-idx view idx)
  (+ idx (vector-view-offset view)))

(define (vector-view-ref view idx)
  (vector-ref (vector-view-vec view) (vector-view-idx view idx)))

(define (vector-view-set! view idx value)
  (vector-set! (vector-view-vec view) (vector-view-idx view idx) value))
