#lang racket/base

(provide shen-function-bindings
         shen-variable-bindings)

(define shen-function-bindings (make-hasheq))
(define shen-variable-bindings (make-hasheq))
