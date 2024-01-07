#lang racket

(require (only-in racket
                  [map r:map]
                  [eval r:eval]
                  [load r:load])
         (only-in racket/exn
                  exn->string)
         (only-in "expander.rkt"
                  package-list
                  shen-function-bindings
                  shen-variable-bindings)
         (only-in "failure.rkt"
                  fail
                  fail-if)
         (only-in "macros.rkt"
                  expand-shen-form
                  remove-shen-macro-expander!)
         "namespaces.rkt"
         (only-in "reader.rkt"
                  shen-readtable)
         (for-syntax syntax/parse)
         syntax/strip-context)

(provide (all-defined-out))

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

(define (load filename)
  (define in (open-input-file filename))
  (define expanded-forms
    (parameterize ([current-readtable shen-readtable])
      (for/list ([stx (in-port (curry read-syntax (object-name in)) in)])
        (expand (strip-context (expand-shen-form stx))))))
  (close-input-port in)
  (eval-syntax #`(begin #,@expanded-forms))
  'loaded)
