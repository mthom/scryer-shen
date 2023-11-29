#lang racket

(require "lang/reader.rkt" "lang/expander.rkt"
         (only-in racket
                  global-port-print-handler
                  [define r:define])
         (for-syntax racket/base racket/provide-transform syntax/parse syntax/stx)
         syntax/strip-context)

(provide (all-from-out "lang/reader.rkt" "lang/expander.rkt")
          #%module-begin)

(begin-for-syntax
  (define-syntax-class curry-out-export
    #:attributes (func-id renamed-id wrapper)
    (pattern [(~seq func-id:id (~optional renamed-id:id #:defaults ([renamed-id #'func-id]))
                    #:arity wrapped-arity:nat)]
      #:with wrapper #'(curry (procedure-reduce-arity func-id wrapped-arity)))
    (pattern [(~seq func-id:id renamed-id:id)]
      #:with wrapper #'(curry func-id))
    (pattern func-id:id
      #:with renamed-id #'func-id
      #:with wrapper #'(curry func-id))))

(define-syntax curry-out
  (make-provide-pre-transformer
   (lambda (stx modes)
     (syntax-parse stx
       [(_ f:curry-out-export ...)
        #:with (curried-f ...)
               (stx-map
               syntax-local-lift-expression
               #'(f.wrapper ...))
        (pre-expand-export
         #'(rename-out [curried-f f.renamed-id] ...)
         modes)]))))

;; list and tuples
(r:define (@p . args) (cons '@p args))

;; arithmetic operators
(provide (curry-out + - * / > < = >= <=))

(provide (rename-out [car hd] [cdr tl])
         (curry-out cons [cons adjoin] append [map #:arity 2])
         @p)
