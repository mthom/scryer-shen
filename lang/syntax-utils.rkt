#lang racket

(require racket
         (for-template
          racket/base
          racket/match
          racket/stxparam
          "failure.rkt"
          (only-in racket/function
                   curry))
         racket/match
         racket/syntax
         syntax/parse
         syntax/stx)

(provide function-clause-definition
         kl-defun
         macro-clause-definition
         quote-pattern
         shen-binding
         shen-var-id
         shen-curry-out-export
         shen-function-out-export
         shen-define
         shen-defmacro
         shen-internal-package
         shen-package
         shen-lambda-form
         shen-let-form)

(define (capitalized-symbol? symbol)
  (and (symbol? symbol)
       (let ([string (symbol->string symbol)])
         (char-upper-case? (string-ref string 0)))))

(define (quote-pattern pattern)
  (syntax-parse pattern
    [((~datum cons) hd tl)
     (quasisyntax/loc pattern
       (cons #,(quote-pattern #'hd)
             #,(quote-pattern #'tl)))]
    [() #''()]
    [(~datum _) pattern]
    [id:id
     #:when (not (capitalized-symbol? (syntax->datum #'id)))
     #''id]
    [_ pattern]))

(define-splicing-syntax-class shen-lambda-form
  (pattern (~seq var:shen-var-id ... body-expr:expr ...+)))

(define-splicing-syntax-class shen-let-form
  #:attributes ((binding-id 1)
                (binding-expr 1)
                (body-expr 1))
  (pattern (~seq binding:shen-binding ...+ body-expr:expr ...+)
           #:with (binding-id ...)   #'(binding.id ...)
           #:with (binding-expr ...) #'(binding.expr ...)))

(define-syntax-class shen-var-id
  (pattern (~and id:id (~fail #:unless (capitalized-symbol? (syntax->datum #'id))))))

(define-syntax-class function-clause-pattern
  #:attributes (pat)
  #:datum-literals (-> <-)
  (pattern (~and (~not ->) (~not <-))
           #:with pat (quote-pattern this-syntax))
  (pattern (~or (->) (<-))
           #:with pat this-syntax))

(define-syntax-class macro-clause-pattern
  #:attributes (pat)
  #:datum-literals (->)
  (pattern (~not ->)
           #:with pat (quote-pattern this-syntax)))

(define-syntax-class shen-op-assoc
  (pattern (~or #:right #:left)))

(define-splicing-syntax-class macro-clause-definition
  #:attributes (body match-clause pat)
  #:datum-literals (->)
  (pattern (~seq pat:macro-clause-pattern -> body:expr)
           #:with match-clause #'[pat.pat body]))

(define-splicing-syntax-class shen-binding
  #:attributes (id expr)
  (pattern (~seq id:shen-var-id expr:expr)))

(define-splicing-syntax-class function-clause-definition
  #:attributes ((pats 1) arrow body guard match-clause)
  #:datum-literals (-> <- where)
  (pattern (~seq pats:function-clause-pattern ... -> body:expr
                 (~optional (~seq where guard:expr)))
           #:with arrow #'->
           #:with match-clause #'[(pats.pat ...) (~? (~@ . (#:when guard))) body])
  (pattern (~seq pats:function-clause-pattern ... <- body:expr
                 (~optional (~seq where guard:expr)))
           #:with arrow #'<-
           #:with match-clause #'[(pats.pat ...)
                                  (=> backtrack-fn)
                                  (~? (unless guard (backtrack-fn)))
                                  (syntax-parameterize ([fail (syntax-id-rules (backtrack-fn)
                                                                [(fail) (backtrack-fn)]
                                                                [fail fail])]
                                                        [fail-if (syntax-id-rules (backtrack-fn)
                                                                   [(fail-if fail-fn r)
                                                                    (let ([result r])
                                                                      (if (fail-fn result)
                                                                          (backtrack-fn)
                                                                          result))]
                                                                   [fail-if
                                                                    (lambda (e r)
                                                                      (let ([result r])
                                                                        (if (fail-fn result)
                                                                            (backtrack-fn)
                                                                            result)))])])
                                    body)]))

(define-splicing-syntax-class shen-define
  #:attributes (name (clause 1) wrapper)
  (pattern (~seq name:id clause:function-clause-definition ...+)
           #:fail-unless (apply = (map length (attribute clause.pats)))
           "each clause must contain the same number of patterns"
           #:with pats (attribute clause.pats)
           #:with (arg-id ...) (stx-map
                                (lambda (stx) (syntax-property stx 'bound #t))
                                (generate-temporaries (car (attribute clause.pats))))
           #:with wrapper #'(curry
                             (lambda (arg-id ...)
                               (match* (arg-id ...)
                                 clause.match-clause ...)))))

(define-splicing-syntax-class kl-defun
  #:attributes (name wrapper (body-expr 1))
  (pattern (~seq name:id (args:shen-var-id ...) body-expr:expr ...+)
           #:do [(stx-map
                  (lambda (stx) (syntax-property stx 'bound #t))
                  #'(args ...))]
           #:with wrapper #'(curry
                             (lambda (args ...)
                               body-expr ...))))

(define-splicing-syntax-class shen-defmacro
  #:attributes (name (pat 1) (clause-expr 1) expander)
  (pattern (~seq name:id clause:macro-clause-definition ...+)
           #:with arg-id (generate-temporary "form")
           #:with (pat ...) #'(clause.pat ...)
           #:with (clause-expr ...) #'(clause.body ...)
           #:with expander #'(lambda (k)
                               (lambda (arg-id)
                                 (match arg-id
                                   clause.match-clause
                                   ...
                                   [_ (k arg-id)])))))

(define-syntax-class shen-top-level-decl
  #:datum-literals (define defun defmacro)
  #:attributes (name expansion)
  (pattern (define define-form:shen-define)
           #:with expansion #'(define . define-form)
           #:with name (attribute define-form.name))
  (pattern (defun defun-form:kl-defun)
           #:with expansion #'(defun . defun-form)
           #:with name (attribute defun-form.name))
  (pattern (defmacro defmacro-form:shen-defmacro)
           #:with expansion #'(defmacro . defmacro-form)
           #:with name (attribute defmacro-form.name)))

(define-splicing-syntax-class shen-package
  #:attributes (export-list
                name
                (top-level-decls 1))
  (pattern (~seq name:id
                 export-list:expr
                 top-level-decls:shen-top-level-decl ...)))

(define-splicing-syntax-class shen-internal-package
  #:attributes (export-list
                external-symbols
                internal-symbols
                name
                (top-level-decls 1))
  (pattern (~seq name:id
                 export-list:expr
                 external-symbols:expr
                 internal-symbols:expr
                 top-level-decls:shen-top-level-decl ...)))

(define-syntax-class shen-curry-out-export
  #:attributes (func-id renamed-id wrapper assoc)
  (pattern [(~seq func-id:id (~optional renamed-id:id #:defaults ([renamed-id #'func-id]))
                  #:arity wrapped-arity:nat
                  (~optional (~seq #:polyadic assoc:shen-op-assoc) #:defaults ([assoc #'#f])))]
           #:fail-when (and (syntax->datum (attribute assoc))
                            (not (= (syntax->datum (attribute wrapped-arity)) 2)))
           "polyadic functions must have arity 2"
           #:with wrapper #'(curry (procedure-reduce-arity func-id wrapped-arity)))
  (pattern [(~seq func-id:id renamed-id:id)]
           #:with wrapper #'(curry func-id)
           #:with assoc #'#f)
  (pattern func-id:id
           #:with renamed-id #'func-id
           #:with wrapper #'(curry func-id)
           #:with assoc #'#f))

(define-syntax-class shen-function-out-export
  #:attributes (func-id renamed-id)
  (pattern [(~seq func-id:id renamed-id:id)])
  (pattern func-id:id
           #:with renamed-id #'func-id))
