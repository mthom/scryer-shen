#lang racket

(require "bindings.rkt"
         (only-in racket
                  [foldr r:foldr]
                  [map r:map]
                  [eval r:eval]
                  [load r:load]
                  [vector r:vector])
         (only-in racket/exn
                  exn->string)
         (only-in "failure.rkt"
                  fail
                  fail-if)
         "macros.rkt"
         "namespaces.rkt"
         (only-in "packages.rkt"
                  package-list)
         (for-syntax syntax/parse)
         syntax/parse/define
         "type-check.rkt")

(provide (all-defined-out))

(define-syntax tc
  (syntax-parser
    [(_ (~datum +))
     #'(begin (type-check? #t) #t)]
    [(_ (~datum -))
     #'(begin (type-check? #f) #f)]
    [_ (raise-syntax-error 'tc "expects + or -")]))

(define-syntax fn
  (syntax-parser
    [(_ id:id)
     #:with fs-proc ((make-interned-syntax-introducer 'function) #'id)
     #'fs-proc]))

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

(define-syntax shen-and
  (syntax-parser
    [(_ a) #'(and-wrapper a)]
    [(_ a b) #'(and-wrapper a (thunk (if b #t #f)))]
    [(_ a b . cs) #'(and-wrapper a (thunk (shen-and b . cs)))]
    [_:id #'and-wrapper]))

(define-syntax shen-or
  (syntax-parser
    [(_ a) #'(or-wrapper a)]
    [(_ a b) #'(or-wrapper a (thunk (if b #t #f)))]
    [(_ a b . cs) #'(or-wrapper a (thunk (shen-or b . cs)))]
    [_:id #'or-wrapper]))

(define-syntax set
  (syntax-parser
    [(_ id:id value:expr)
     #'(hash-set! shen-variable-bindings id value)]
    [id:id #''id]))

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

(define (foldr fn acc list)
  (r:foldr (function fn) acc list))

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
              shen-namespace
              kl-namespace)))

(define (eval expr)
  (r:eval expr shen-namespace))

(define error-to-string exn->string)

(define (concat x y)
  (if (and (symbol? x) (symbol? y))
      (string->symbol (string-append (symbol->string x) (symbol->string y)))
      (and x y)))

(define (undefmacro name)
  (if (symbol? name)
      (remove-shen-macro-expander! name)
      (error "undefmacro: argument must be a symbol naming a macro.")))

(define (external pkg-name)
  (package-list pkg-name 'external))

(define (internal pkg-name)
  (package-list pkg-name 'internal))

(define (output fmt-string . args)
  (apply printf fmt-string args))

(define (vector size)
  (build-vector size (const '...)))

(define-syntax-parse-rule (freeze stx:expr)
  (thunk stx))

(define (thaw f) (f))

;; Shen vectors are 1-indexed while Racket's are 0-indexed so sub1 to
;; compensate

(define (<-vector vec idx)
  (vector-ref vec (sub1 idx)))

(define (vector-> vec idx value)
  (vector-set! vec (sub1 idx) value)
  vec)

(define limit vector-length)
