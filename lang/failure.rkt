#lang racket

(require racket/stxparam)

(provide (protect-out fail fail-if failure-object))

(define-values (struct:failure make-failure-object failure? failure-ref set-failure!)
  (make-struct-type 'failure #f 0 0))

(define failure-object (make-failure-object))

(define fail-if-fn
  (curry
   (lambda (fail-fn x)
     (if (fail-fn x)
         failure-object
         x))))

(define-syntax-parameter fail
  (syntax-id-rules (fail)
    [(fail) failure-object]
    [fail (lambda () failure-object)]))

(define-syntax-parameter fail-if
  (syntax-id-rules (fail)
    [(fail-if fail-fn x) (fail-if-fn fail-fn x)]
    [fail-if fail-if-fn]))