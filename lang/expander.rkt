#lang racket

(require "bindings.rkt"
         "macros.rkt"
         "namespaces.rkt"
         racket/stxparam
         "packages.rkt"
         "pairs.rkt"
         "prolog.rkt"
         syntax/parse/define
         "systemf.rkt"
         "type-syntax-expanders.rkt"
         (for-syntax "prolog-syntax-expanders.rkt"
                     racket/base
                     racket/function
                     racket/match
                     racket/port
                     racket/provide-transform
                     racket/stxparam
                     racket/syntax
                     (only-in "reader.rkt"
                              shen-readtable)
                     syntax/parse
                     syntax/stx
                     "syntax-utils.rkt"
                     "types-syntax.rkt"))

(define-for-syntax (variadic-op-macros name assoc)
  (syntax-parse assoc
    [(~datum #:right)
     #:with right-assoc-macro-id (format-id name "shen.~a-right-assoc-macro" name)
     (syntax-local-lift-module-end-declaration
      #`(shen-defmacro right-assoc-macro-id
          (cons #,name (cons Op1 (cons Op2 (cons Op3 Ops))))
          ->
          (cons '#,name (cons Op1 (cons (cons '#,name (cons Op2 (cons Op3 Ops))) '())))))]
    [(~datum #:left)
     #:with left-assoc-macro-id (format-id name "shen.~a-left-assoc-macro" name)
     #`(shen-defmacro left-assoc-macro-id
         (cons #,name (cons Op1 (cons Op2 (cons Op3 Ops))))
         ->
         (cons '#,name (cons (cons '#,name (cons Op1 (cons Op2 '()))) (cons Op3 Ops))))]))

(define-syntax define-shen-function
  (syntax-parser
    [(_ fn:id racket-fn)
     #:with spaced-fn ((make-interned-syntax-introducer 'function) #'fn)
     #'(begin
         (define spaced-fn (procedure-rename racket-fn 'fn 'shen))
         (hash-set! shen-function-bindings 'fn spaced-fn))]))

(define-syntax shen-curry-out
  (make-provide-pre-transformer
   (lambda (export-list modes)
     (syntax-parse export-list
       [(_ . export-list)
        #:with (export ...)
        (stx-map (syntax-parser
                   [f:shen-curry-out-export
                    (when (syntax->datum #'f.assoc)
                      (variadic-op-macros #'f.renamed-id #'f.assoc))
                    (syntax-local-lift-expression
                     #'(systemf 'f.renamed-id))
                    (syntax-local-lift-module-end-declaration
                     #'(define-shen-function f.renamed-id f.wrapper))
                    #'(for-space function f.renamed-id)])
                 (syntax->list #'export-list))
       (pre-expand-export
        #'(combine-out export ...)
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
     #:when (attribute define-form.type-sig)
     #:cut
     (let-values ([(queries assert-strings retract-strings)
                   (function-def->type-check-queries
                    #'define-form.name
                    #'define-form.type-sig
                    #'(define-form.clause ...))])
       #`(begin
           (define-shen-function define-form.name define-form.wrapper)
           (enqueue-function-type-data! '#,queries '#,assert-strings '#,retract-strings)
           define-form.name))]
    [(shen-define define-form:shen-define)
     #'(begin
         (define-shen-function define-form.name define-form.wrapper)
         define-form.name)]
    [shen-define:id #''define]))

(define-syntax shen-defmacro
  (syntax-parser
    [(shen-defmacro defmacro:shen-defmacro)
     #'(add-shen-macro-expander! 'defmacro.name defmacro.expander)]
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

(define-match-expander @p
  (syntax-parser
    [((~literal @p) arg1 arg2 ... args)
     #'(shen-tuple (vector arg1 arg2 ... args))])
  (syntax-parser
    [((~literal @p) arg1 arg2 ... args)
     #'(shen-tuple (vector arg1 arg2 ... args))]))

(define (@s-pattern num-chars)
  (lambda (str)
    (for/list ([ch (in-string str)]
               [n  (in-range 0 (add1 num-chars))])
      (if (= n num-chars)
          (substring str num-chars)
          (string ch)))))

(define-match-expander @s
  (syntax-parser
    [((~literal @s) (~or arg:shen-var-id arg:unit-string) ...+
                    (~or last-arg:shen-var-id last-arg:string))
     #:with num-chars (length (syntax->list #'(arg ...)))
     #'(? string?
          ;; quote is needed here to compensate for lack of #%datum in
          ;; the transformer phase
          (app (@s-pattern (quote num-chars))
               (list arg ... last-arg)))])
  (syntax-parser
    [((~literal @s) arg1 args ...+)
     #'(string-append arg1 args ...)]))

(define (@v-pattern num-elts)
  (lambda (vec)
    (vector-append (vector-take vec num-elts)
                   (vector (vector-drop vec num-elts)))))

(define-match-expander @v
  (syntax-parser
    [((~literal @v) arg ...+ (~literal <>))
     #:with num-elts (length (syntax->list #'(arg ...)))
     #'(vector arg ...)]
    [((~literal @v) arg ...+ last-arg)
     #:with num-elts (length (syntax->list #'(arg ...)))
     #'(? vector?
          (app (@v-pattern (quote num-elts))
               (vector arg ... last-arg)))])
  (syntax-parser
    [((~literal @v) arg ...+ last-arg)
     #'(vector-append (vector arg ...) last-arg)]))

(define-syntax shen-lambda
  (syntax-parser
    [(shen-lambda lambda-form:shen-lambda-form)
     #'(procedure-rename (curry
                          (lambda (lambda-form.var ...)
                            lambda-form.body-expr))
                         'anonymous-fn
                         'shen)]))

(define-syntax shen-let
  (syntax-parser
    [(shen-let let-form:shen-let-form)
     #'(let* ([let-form.binding-id let-form.binding-expr]
              ...)
         let-form.body-expr)]))

(define-syntax shen-cond
  (syntax-parser
    [(shen-cond cond-form:shen-cond-form)
     #'(cond [cond-form.condition cond-form.true-form]
             ...
             [else #f])]))

(define-syntax shen-datatype
  (syntax-parser
    [(shen-datatype type-module-name:id sequent:shen-type-sequent ...+)
     #:with iso-prolog-type-definitions (datatype->type-definition
                                         #'type-module-name
                                         #'(sequent ...))
     #'(enqueue-datatype-definition! (quote iso-prolog-type-definitions))]))

(define-syntax shen-if
  (syntax-parser
    [(shen-if if-form:shen-if-form)
     #'(if if-form.condition if-form.true-form if-form.false-form)]))

(define-syntax (shen-true stx)
  (syntax-case stx ()
    [_:id #'#t]))

(define-syntax (shen-false stx)
  (syntax-case stx ()
    [_:id #'#f]))

(define-syntax <>
  (syntax-parser
    [_:id #'#()]))

(provide (protect-out <>
                      @p
                      @s
                      @v
                      kl-defun
                      shen-curry-out
                      shen-function-out)
         (rename-out [shen-true true]
                     [shen-false false]
                     [shen-datatype datatype]
                     [shen-define define]
                     [shen-cond cond]
                     [shen-if if]
                     [shen-let let]
                     [shen-lambda /.]
                     [shen-defmacro defmacro]
                     [shen-defprolog defprolog]
                     [shen-package package]
                     [shen-prolog? prolog?]))
