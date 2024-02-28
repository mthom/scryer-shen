#lang racket/base

(provide type-check?)

(define type-check?
  (make-parameter #f (lambda (val)
                       (unless (boolean? val)
                         (raise-type-error 'tc "boolean" val))
                       val)))
