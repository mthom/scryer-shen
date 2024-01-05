#lang racket

(require "macros.rkt"
         "namespaces.rkt"
         racket/stxparam
         "systemf.rkt"
         syntax/parse/define
         (for-syntax "packages.rkt"
                     racket/base
                     racket/function
                     racket/match
                     racket/provide-transform
                     racket/stxparam
                     racket/syntax
                     syntax/parse
                     syntax/stx
                     "syntax-utils.rkt"))

(define shen-function-bindings (make-hasheq))
(define shen-variable-bindings (make-hasheq))
(define shen-packages (make-hasheq))

(define (package-list pkg-name [type 'external])
  (let ([package-ht (hash-ref! shen-packages pkg-name (thunk (make-hasheq)))])
    (for/list ([(symbol symbol-type) (in-hash package-ht)]
               #:when (eq? symbol-type type))
      symbol)))

(define (add-internal-symbols-to-package! pkg-name internal-symbols-list)
  (let ([package-ht (hash-ref! shen-packages pkg-name (thunk (make-hasheq)))])
    (map (lambda (symbol)
           (hash-set! package-ht symbol 'internal))
         internal-symbols-list)))

(define (add-external-symbols-to-package! pkg-name external-symbols-list)
  (let ([package-ht (hash-ref! shen-packages pkg-name (thunk (make-hasheq)))])
    (map (lambda (symbol)
           (hash-set! package-ht symbol 'external))
         external-symbols-list)))

(begin-for-syntax
  (define (generate-variadic-macro-or-wrapper renamed-id assoc wrapper-id
                                              [new-id (generate-temporary "wrapper-macro")])
    (case assoc
      [(#:right) (with-syntax ([wrapper-id wrapper-id]
                               [new-id new-id]
                               [renamed-id renamed-id])
                   (syntax-local-lift-module-end-declaration
                    #'(define-syntax new-id
                        (syntax-parser
                           [(_ a) #'(wrapper-id a)]
                           [(_ a b) #'(wrapper-id a b)]
                           [(_ a b . cs) #'(wrapper-id a (new-id b . cs))]
                           [_:id #'renamed-id]))))
                 new-id]
      [(#:left) (with-syntax ([wrapper-id wrapper-id]
                              [new-id new-id]
                              [renamed-id renamed-id])
                  (syntax-local-lift-module-end-declaration
                   #'(define-syntax new-id
                        (syntax-parser
                           [(_ a) #'(wrapper-id a)]
                           [(_ a b) #'(wrapper-id a b)]
                           [(_ a b . cs) #'(wrapper-id a (new-id b . cs))]
                           [_:id #'renamed-id]))))
                new-id]
      [else wrapper-id])))

(define-syntax define-shen-function
  (syntax-parser
    [(_ fn:id racket-fn)
     #:with spaced-fn ((make-interned-syntax-introducer 'function) #'fn)
     #:with non-spaced-fn (generate-temporary "non-spaced-wrapper")
     #'(begin
         (define non-spaced-fn racket-fn)
         (define spaced-fn non-spaced-fn)

         (hash-set! shen-function-bindings 'fn non-spaced-fn)

         (namespace-set-variable-value! 'fn non-spaced-fn #t kl-namespace)
         (namespace-set-variable-value! 'fn non-spaced-fn #t shen-namespace))]))

(define-syntax shen-curry-out
  (make-provide-pre-transformer
   (lambda (stx modes)
     (syntax-parse stx
       [(_ f:shen-curry-out-export ...)
        #:with (wrapper-f ...)
               (stx-map
                (lambda (stx)
                  (let* ([old-wrapper-id (generate-temporary "wrapper")]
                         [new-wrapper-id ((make-interned-syntax-introducer 'function) old-wrapper-id)])
                    (with-syntax ([wrapper-id old-wrapper-id]
                                  [stx stx])
                      (syntax-local-lift-module-end-declaration
                       #'(define-shen-function wrapper-id stx)))
                    new-wrapper-id))
                #'(f.wrapper ...))
        #:with (exports ...)
               (stx-map
                (lambda (stx)
                  (match (syntax-e stx)
                    [(list assoc renamed-id curry-wrapper wrapper-id)
                     (with-syntax ([macro-or-wrapper-id (generate-variadic-macro-or-wrapper
                                                         (syntax->datum renamed-id)
                                                         (syntax->datum assoc)
                                                         wrapper-id)]
                                   [curry-wrapper curry-wrapper]
                                   [renamed-id renamed-id])
                       (syntax-local-lift-expression
                        #'(systemf 'renamed-id))
                       (syntax-local-lift-expression
                        #'(hash-set! shen-function-bindings 'renamed-id curry-wrapper))
                       (if (syntax->datum assoc)
                           #'(rename-out [macro-or-wrapper-id renamed-id])
                           #'(for-space function (rename-out [macro-or-wrapper-id renamed-id]))))]))
                #'((f.assoc f.renamed-id f.wrapper wrapper-f) ...))
        (pre-expand-export
         #'(combine-out exports ...)
         modes)]))))

(define-syntax shen-function-out
  (make-provide-pre-transformer
   (lambda (stx modes)
     (syntax-parse stx
       [(_ f:shen-function-out-export ...)
        #:do [(stx-map
               syntax-local-lift-module-end-declaration
               #'((define-shen-function f.renamed-id f.func-id) ...))
              (stx-map
               syntax-local-lift-expression
               #'((systemf 'f.renamed-id) ...))]
        (pre-expand-export
         #'(for-space function f.renamed-id ...)
         modes)]))))

(define-syntax (shen-define stx)
  (syntax-parse stx
    [(shen-define define-form:shen-define)
     #'(define-shen-function define-form.name define-form.wrapper)]
    [shen-define:id #''shen-define]))

(define-syntax (shen-defmacro stx)
  (syntax-parse stx
    [(shen-defmacro defmacro:shen-defmacro)
     #'(add-shen-macro-expander! defmacro.name defmacro.expander)]
    [defmacro:id #''defmacro]))

(define-syntax (shen-package stx)
  (syntax-parse stx
    [(shen-package package:shen-package)
     (let-values ([(top-level-forms external-symbols internal-symbols)
                   (unpackage-shen-package
                    #'package.name
                    #'package.export-list
                    #'(package.top-level-decls ...))])
       (with-syntax ([external-symbols (hash-keys external-symbols)]
                     [internal-symbols (hash-keys internal-symbols)])
         (syntax-local-lift-expression
          #'(add-external-symbols-to-package!
             'package.name
             'external-symbols))
         (syntax-local-lift-expression
          #'(add-internal-symbols-to-package!
             'package.name
             'internal-symbols))
         #'(begin package.top-level-decls ...)))]))

(define-syntax (kl-defun stx)
  (syntax-parse stx
    [(kl-defun defun:kl-defun)
     #'(define-shen-function defun.name defun.wrapper)]
    [defun:id #''defun]))

(define-syntax shen-lambda
  (syntax-parser
    [(shen-lambda lambda-form:shen-lambda-form)
     #'(lambda (lambda-form.var ...)
         lambda-form.body-expr
         ...)]))

(define-syntax shen-let
  (syntax-parser
    [(shen-let let-form:shen-let-form)
     #'(let* ([let-form.binding-id let-form.binding-expr]
              ...)
         let-form.body-expr
         ...)]))

(define-syntax (shen-true stx)
  (syntax-case stx ()
    [_:id #'#t]))

(define-syntax (shen-false stx)
  (syntax-case stx ()
    [_:id #'#f]))

(provide package-list
         (protect-out kl-defun
                      shen-curry-out
                      shen-function-out
                      shen-function-bindings
                      shen-variable-bindings)
         (rename-out [shen-true true]
                     [shen-false false]
                     [shen-define define]
                     [shen-let let]
                     [shen-lambda /.]
                     [shen-defmacro defmacro]
                     [shen-package package]))
