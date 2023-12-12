#lang racket

(require (for-syntax syntax/parse
                     "syntax-utils.rkt")
         syntax/parse/define
         (only-in "expander.rkt"
                  curry-out
                  shen-variable-namespace))

(define (and-wrapper value thunk)
  (if value (thunk) #f))

(define (or-wrapper value thunk)
  (if value #t (thunk)))

(define-syntax (shen-and stx)
  (syntax-parse stx
    [(_ a b) #'(and-wrapper a (thunk (if b #t #f)))]
    [(_ a b . cs) #'(and-wrapper a (thunk (shen-and b . cs)))]
    [_:id #'and-wrapper]))

(define-syntax (shen-or stx)
  (syntax-parse stx
    [(_ a b) #'(or-wrapper a (thunk (if b #t #f)))]
    [(_ a b . cs) #'(or-wrapper a (thunk (shen-or b . cs)))]
    [_:id #'or-wrapper]))

(define-syntax-parse-rule (shen-set id:id value:expr)
  (namespace-set-variable-value! 'id value #f shen-variable-namespace))

(define-syntax (shen-value stx)
  (syntax-parse stx
    [(_ id:id) #'(value-fn 'id)]
    [id:id #'value-fn]))

(define (value-fn v)
  (if (symbol? v)
      (namespace-variable-value v #t (thunk (error "name not found in symbol table."))
                                shen-variable-namespace)
      (error "value: first parameter must be a symbol.")))

;; list and tuples
;; (define (@p . args) (cons '@p args))

(define (arity proc)
  (let ([arity (procedure-arity proc)])
    (if (cons? arity)
        (last arity)
        arity)))

(define (bound? value)
  (if (symbol? value)
      (with-handlers ([exn:fail:contract:variable? (const #f)])
        (namespace-variable-value value)
        #t)
      #f))

;; system functions manifest
(provide (curry-out [+ #:arity 2 #:polyadic #:right]
                    [* #:arity 2 #:polyadic #:right]
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
         (rename-out [car hd]
                     [cdr tl]
                     [vector? absvector?]
                     [shen-set set]
                     [shen-value value]
                     [shen-and and]
                     [shen-or or])
         fail
         fail-if)
