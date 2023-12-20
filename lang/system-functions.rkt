#lang racket

(require racket/base
         (only-in racket
                  [map r:map])
         "syntax-utils.rkt"
         (for-syntax syntax/parse)
         syntax/parse/define
         (only-in "expander.rkt"
                  curry-out
                  shen-function-out
                  shen-function-bindings
                  shen-variable-bindings))

#|
;; list and tuples
;; (define (@p . args) (cons '@p args))
|#

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

(define (arity proc)
  (let ([arity (procedure-arity proc)])
    (if (cons? arity)
        (last arity)
        arity)))

(define (bound? var)
  (if (symbol? var)
      (with-handlers ([exn:fail:contract? (thunk #f)])
        (begin
          (hash-ref shen-variable-bindings var)
          #t))
      (error "bound?: first parameter must be a symbol.")))

(define-syntax set
  (syntax-parser
    [(_ id:id value:expr)
     #'(hash-set! shen-variable-bindings id value)]))

(define (value key)
  (if (symbol? key)
      (hash-ref shen-variable-bindings key)
      (error "value: first parameter must be a symbol.")))

(define (function var)
  (if (symbol? var)
      (hash-ref shen-function-bindings var)
      (error "function: first parameter must be a symbol.")))

(define (map fn list)
  (r:map (if (procedure? fn) fn (function fn)) list))

(define (cd string)
  (current-directory string))

(define cn string-append)

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
                    [map #:arity 2]
                    [cn #:arity 2])
         (rename-out [shen-and and]
                     [shen-or or])
         (shen-function-out [car hd]
                            cd
                            [cdr tl]
                            [vector? absvector?]
                            arity
                            bound?
                            function
                            symbol?
                            value)
         set
         fail
         fail-if)