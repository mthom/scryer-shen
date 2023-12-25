#lang racket

(require (only-in racket
                  [map r:map]
                  [eval r:eval])
         (only-in racket/exn
                  exn->string)
         (only-in "expander.rkt"
                  curry-out
                  shen-function-out
                  shen-function-bindings
                  shen-variable-bindings)
         (only-in racket/syntax
                  format-id)
         (for-syntax syntax/parse)
         "namespaces.rkt"
         "syntax-utils.rkt"
         syntax/parse/define)

(define-syntax destroy
  (syntax-parser
    [(_ id:id)
     #`(begin
         (set! #,((make-interned-syntax-introducer 'function) #'id) (void))
         (namespace-undefine-variable! 'id shen-namespace)
         (namespace-undefine-variable! 'id kl-namespace)
         (hash-remove! shen-function-bindings id))]
    [(_ value) #'value]
    [id:id #''id]))

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

(define (difference list-1 list-2)
  (filter (lambda (e) (not (member e list-2 equal?))) list-1))

(define (element? e list)
  (member e list equal?))

(define (eval-kl expr)
  (r:eval expr
          (if (and (cons? expr) (eq? 'defun (first expr)))
              (current-namespace)
              kl-namespace)))

(define (eval expr)
  (r:eval expr))

;; system functions manifest
(provide (curry-out [+ #:arity 2 #:polyadic #:right]
                    [* #:arity 2 #:polyadic #:right]
                    [- #:arity 2]
                    [/ #:arity 2]
                    [> #:arity 2]
                    [< #:arity 2]
                    [equal? = #:arity 2]
                    [equal? == #:arity 2]
                    [>= #:arity 2]
                    [<= #:arity 2]
                    [make-vector absvector #:arity 1]
                    [vector-ref <-address #:arity 2]
                    [vector-set! address-> #:arity 3]
                    [cons #:arity 2]
                    [cons adjoin #:arity 2]
                    [append #:arity 2]
                    [map #:arity 2]
                    [cn #:arity 2]
                    [difference #:arity 2]
                    [element? #:arity 2])
         (rename-out [begin do]
                     [shen-and and]
                     [shen-or or])
         (shen-function-out [car hd]
                            cd
                            [cdr tl]
                            [vector? absvector?]
                            [exn->string error-to-string]
                            arity
                            bound?
                            cons?
                            empty?
                            eval
                            eval-kl
                            function
                            symbol?
                            value)
         destroy
         error
         fail
         fail-if
         set)
