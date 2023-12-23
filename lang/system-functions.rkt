#lang racket

(require racket/base
         (only-in racket
                  [map r:map])
         "syntax-utils.rkt"
         (for-syntax syntax/id-table
                     syntax/parse)
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

(define-syntax destroy
  (syntax-parser
    [(_ id:id)
     #`(begin
         (set! #,((make-interned-syntax-introducer 'function) #'id) (void))
         (hash-remove! shen-function-bindings id))]
    [(_ value) #'value]))

(define and-wrapper
  (curry
   (lambda (value thunk)
     (if value (thunk) #f))))

(define or-wrapper
  (curry
   (lambda (value thunk)
     (if value #t (thunk)))))

(define-syntax (shen-and stx)
  (syntax-parse stx
    [(_ a) #'(and-wrapper a)]
    [(_ a b) #'(and-wrapper a (thunk (if b #t #f)))]
    [(_ a b . cs) #'(and-wrapper a (thunk (shen-and b . cs)))]
    [_:id #'and-wrapper]))

(define-syntax (shen-or stx)
  (syntax-parse stx
    [(_ a) #'(or-wrapper a)]
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
  (cond [(symbol? var)
         (hash-ref shen-function-bindings var)]
        [(procedure? var)
         var]
        [else
         (error "function: first parameter must be bound to a function.")]))

(define (map fn list)
  (r:map (function fn) list))

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
         destroy
         set
         fail
         fail-if)
