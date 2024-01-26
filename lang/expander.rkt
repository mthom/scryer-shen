#lang racket

(require "bindings.rkt"
         "macros.rkt"
         "namespaces.rkt"
         racket/stxparam
         "packages.rkt"
         "prolog.rkt"
         "systemf.rkt"
         syntax/parse/define
         (for-syntax racket/base
                     racket/function
                     racket/match
                     racket/provide-transform
                     racket/stxparam
                     racket/syntax
                     syntax/parse
                     syntax/stx
                     "syntax-utils.rkt"))

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

(define-syntax shen-define
  (syntax-parser
    [(shen-define define-form:shen-define)
     #'(define-shen-function define-form.name define-form.wrapper)]
    [shen-define:id #''shen-define]))

(define-syntax shen-defmacro
  (syntax-parser
    [(shen-defmacro defmacro:shen-defmacro)
     #'(add-shen-macro-expander! defmacro.name defmacro.expander)]
    [defmacro:id #''defmacro]))

(define-syntax shen-package
  (syntax-parser
    [(shen-package package:shen-package)
     #:when (and (eq? (syntax->datum #'package.name) 'null)
                 (eq? (syntax->datum #'package.export-list) '()))
     #'(begin package.top-level-decls ...)]
    [(shen-package package:shen-internal-package)
     (syntax-local-lift-expression
      #'(add-external-symbols-to-package!
          'package.name
          'package.external-symbols))
     (syntax-local-lift-expression
      #'(add-internal-symbols-to-package!
          'package.name
          'package.internal-symbols))
     #'(begin
         package.top-level-decls ...)]))

(define-syntax shen-defprolog
  (syntax-parser
    [(shen-defprolog rule-name:id rule:shen-prolog-rule ...+)
     #:with iso-prolog-code (datum->syntax #'rule-name
                                           (expand-shen-defprolog #'rule-name #'(rule ...))
                                           #'rule-name)
     #'(add-prolog-predicate! iso-prolog-code)]))

(define-syntax shen-prolog?
  (syntax-parser
    [(shen-prolog? goal:prolog-body-pattern ...+)
     #:with iso-prolog-query (expand-shen-prolog-query #'(goal ...))
     #'(run-prolog-query! iso-prolog-query)]))

(define-syntax kl-defun
  (syntax-parser
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

(provide (protect-out kl-defun
                      shen-curry-out
                      shen-function-out)
         (rename-out [shen-true true]
                     [shen-false false]
                     [shen-define define]
                     [shen-let let]
                     [shen-lambda /.]
                     [shen-defmacro defmacro]
                     [shen-defprolog defprolog]
                     [shen-package package]
                     [shen-prolog? prolog?]))
