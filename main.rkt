#lang racket

(require "lang/reader.rkt" "lang/expander.rkt"
         (only-in racket [define r:define])
         syntax/strip-context)

(provide (all-from-out "lang/reader.rkt" "lang/expander.rkt")         
          #%module-begin)

;; arithmetic operators
(provide + - * /)

;; list and tuples
(r:define hd car)
(r:define tl cdr)
(r:define (@p . args) (cons '@p args))

(provide hd tl @p cons)