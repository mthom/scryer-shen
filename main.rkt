#lang racket

(require "lang/reader.rkt" "lang/expander.rkt"
         (only-in racket
                  global-port-print-handler
                  [define r:define]
                  [let r:let])
         (for-syntax racket/base syntax/parse)
         syntax/strip-context)

(provide (all-from-out "lang/reader.rkt" "lang/expander.rkt")
          #%module-begin)

;; list and tuples
(r:define (@p . args) (cons '@p args))

(r:define (arity proc)
  (r:let ([arity (procedure-arity proc)])
    (if (cons? arity)
        (last arity)
        arity)))

(r:define (bound? value)
  (if (symbol? value)
      (with-handlers ([exn:fail:contract:variable? (const #f)])
        (namespace-variable-value value)
        #t)
      #f))

(compile-allow-set!-undefined #t)

;; system functions
(provide (curry-out [+ #:arity 2 #:variadic #:right]
                    [* #:arity 2 #:variadic #:right]
                    [- #:arity 2]
                    [/ #:arity 2]
                    [> #:arity 2]
                    [< #:arity 2]
                    [= #:arity 2]
                    [= == #:arity 2]
                    [>= #:arity 2]
                    [<= #:arity 2]
                    [make-vector absvector #:arity 1]
                    [vector-ref <-address #:arity 2]
                    [vector-set! address-> #:arity 3]
                    [cons #:arity 2]
                    [cons adjoin #:arity 2]
                    [append #:arity 2]
                    [map #:arity 2])
         (rename-out [boolean? boolean?]
                     [car hd]
                     [cdr tl]
                     [shen-set set]
                     [shen-value value]
                     [vector? absvector?]
                     [shen-and and]
                     [shen-or or])
         arity
         bound?)

(provide @p)