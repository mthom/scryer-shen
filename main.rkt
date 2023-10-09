#lang racket

(require "lang/reader.rkt" "lang/expander.rkt"
         (only-in racket [define r:define])
         syntax/strip-context)

(provide (all-from-out "lang/reader.rkt" "lang/expander.rkt")         
          #%module-begin)

;; arithmetic operators
(provide + - * / > <)

;; list and tuples
(r:define (@p . args) (cons '@p args))

(provide (rename-out [car hd] [cdr tl]) @p cons append map)