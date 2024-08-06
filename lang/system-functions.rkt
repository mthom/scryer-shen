#lang racket

(require "bindings.rkt"
         (only-in "expander.rkt"
                  defmacro)
         (only-in racket
                  [foldr r:foldr]
                  [map r:map]
                  [eval r:eval]
                  [vector r:vector])
         (only-in racket/exn
                  exn->string)
         (only-in "failure.rkt"
                  fail
                  fail-if)
         "macros.rkt"
         "namespaces.rkt"
         "pairs.rkt"
         (only-in "packages.rkt"
                  package-list)
         (for-syntax syntax/parse)
         syntax/parse/define
         "type-syntax-expanders.rkt")

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
         (hash-remove! shen-function-bindings id))]
    [(_ value) #'value]
    [id:id #''id]))

(defmacro shen.and-right-assoc-macro
  (cons and (cons Op1 (cons Op2 (cons Op3 Ops))))
  ->
  (cons 'if (cons Op1 (cons (cons 'and (cons Op2 (cons Op3 Ops))) '(false))))
  (cons and (cons Op1 (cons Ops '())))
  ->
  (cons 'if (cons Op1 (cons (cons 'if (cons Ops '(true false))) '(false)))))

(defmacro shen.or-right-assoc-macro
  (cons or (cons Op1 (cons Op2 (cons Op3 Ops))))
  ->
  (cons 'if (cons Op1 (cons 'true (cons (cons 'or (cons Op2 (cons Op3 Ops))) '()))))
  (cons or (cons Op1 (cons Ops '())))
  ->
  (cons 'if (cons Op1 (cons 'true (cons Ops '())))))

(define-syntax set
  (syntax-parser
    [(_ id:id value:expr)
     #'(hash-set! shen-variable-bindings id value)]
    [id:id #''id]))

(define (arity proc)
  (let ([arity (procedure-arity (function proc))])
    (if (cons? arity)
        (last arity)
        arity)))

(define (bound? var)
  (if (symbol? var)
      (with-handlers ([exn:fail:contract? (lambda (_) #f)])
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
  (procedure-rename (thunk stx) '<thunk>))

(define (thaw f) (f))

;; Shen vectors are 1-indexed while Racket's are 0-indexed so sub1 to
;; compensate

(define (<-vector vec idx)
  (vector-ref vec (sub1 idx)))

(define (vector-> vec idx value)
  (vector-set! vec (sub1 idx) value)
  vec)

(define limit vector-length)

(define (explode data)
  (match data
    [(? string?)
     (map string (string->list data))]
    [(? vector?)
     (append '("<")
             (apply append (add-between (vector->list (vector-map explode data)) '(" ")))
             '(">"))]
    [(? symbol?)
     (explode (symbol->string data))]
    [(? number?)
     (explode (number->string data))]
    [(list args ...)
     (append '("[")
             (apply append (add-between (map explode args) '(" ")))
             '("]"))]
    [(list-rest args ... tail)
     (append '("[")
             (apply append (add-between (map explode args) '(" ")))
             '(" | ")
             (explode tail)
             '("]"))]))

(define (atom? value)
  (or (boolean? value)
      (number? value)
      (string? value)
      (symbol? value)))

(define (fix f)
  (procedure-rename (letrec ([fix-help (lambda (x)
                                         (let ([result (f x)])
                                           (if (equal? result x)
                                               x
                                               (fix-help result))))])
                      fix-help)
                    '<thunk>))

(define (fst tuple)
  (vector-ref (shen-tuple-args tuple) 0))

(define (snd tuple)
  (vector-ref (shen-tuple-args tuple) 1))

(define (hdstr str)
  (string (string-ref str 0)))

(define intern string->symbol)
