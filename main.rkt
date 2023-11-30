#lang racket

(require "lang/reader.rkt" "lang/expander.rkt"
         (only-in racket
                  global-port-print-handler
                  [define r:define])
         (for-syntax racket/base syntax/parse)
         syntax/strip-context)

(provide (all-from-out "lang/reader.rkt" "lang/expander.rkt")
          #%module-begin)

;; list and tuples
(r:define (@p . args) (cons '@p args))

;; arithmetic operators
(provide (curry-out + - * / > < = >= <=))

(provide (rename-out [car hd] [cdr tl])
         (curry-out
          [cons #:arity 2]
          [cons adjoin #:arity 2]
          [append #:arity 2]
          [map #:arity 2])
         @p)
