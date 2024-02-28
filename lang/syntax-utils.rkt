#lang racket

(require data/gvector
         racket
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
         prolog-body-pattern
         quote-pattern
         shen-binding
         shen-cons-syntax
         shen-curry-out-export
         shen-define
         shen-defmacro
         shen-internal-package
         shen-function-out-export
         shen-function-type-sig
         shen-package
         shen-cond-form
         shen-if-form
         shen-lambda-form
         shen-let-form
         shen-prolog-rule
         shen-prolog-term
         shen-special-form?
         shen-var-id
         unit-string)

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

(define-literal-set shen-special-forms
  #:datum-literals (true false let cons if /. cond fn @p @s @v)
  ())

(define shen-special-form?
  (literal-set->predicate shen-special-forms))

(define-syntax-class unit-string
  (pattern str:string
           #:fail-unless (= (string-length (syntax-e #'str)) 1)
           "string literal patterns must consist of a single character"))

(define (syntax->shen-prolog-term stx [type-datum? #f] [untagged-vars? #f])
  (syntax-parse stx
    [(~var term (shen-prolog-term #:type-datum type-datum? #:untagged-vars untagged-vars?))
     #'term.term]))

(define shen-cons-syntax
  (syntax-parser
    [(a . d)
     #`(cons a #,(shen-cons-syntax #'d))]
    [stx
     #'stx]))

(define-splicing-syntax-class shen-function-type-sig
  #:attributes ((type 1))
  #:datum-literals (-->)
  (pattern (~seq t1:shen-prolog-term (~seq --> t2:shen-prolog-term) ...+)
           #:with (type ...) #'(t1.term t2.term ...))
  (pattern (~seq --> t:shen-prolog-term)
           #:with (type ...) #'(t.term)))

(define (wrap-tagged-shen-prolog-term datum-term)
  (define wrapper (syntax-parse-state-ref (syntax->datum datum-term) #f))
  (if (procedure? wrapper)
      (wrapper datum-term)
      datum-term))

(define-syntax-class (shen-prolog-term #:type-datum [type-datum? #f]
                                       #:untagged-vars [untagged-vars? #t])
  #:attributes (term)
  (pattern str:string
           #:with term #'(#%prolog-functor string str))
  (pattern num:number
           #:with term #'(#%prolog-functor number num))
  (pattern sym:shen-var-id
           #:with term (let ([term (if untagged-vars?
                                       #'sym
                                       #`(#%prolog-functor #,(if type-datum? #'symbol #'?) sym))])
                         (wrap-tagged-shen-prolog-term term)))
  (pattern (~and sym:id (~not :shen-var-id))
           #:fail-when (or (shen-special-form? #'sym)
                           (syntax-parse #'sym
                             [(~or (~literal #%prolog-functor)
                                   (~literal apply))
                              #t]
                             [_ #f]))
           #f
           #:with term #'(#%prolog-functor symbol sym))
  (pattern ((~and id:id (~or (~datum @p)
                             (~datum @s)
                             (~datum @v)))
            arg args ...+)
           #:with term (if (null? (stx-cdr #'(args ...)))
                           #'(#%prolog-functor id arg args ...)
                           #`(#%prolog-functor id arg #,(syntax->shen-prolog-term
                                                         #'(id args ...)
                                                         type-datum?
                                                         untagged-vars?))))
  ;; TODO: let, if, /., cond, apply
  ;; strip off quote installed by quote-patterns etc.
  (pattern ((~datum quote)
            (~var x (shen-prolog-term #:type-datum type-datum? #:untagged-vars untagged-vars?)))
           #:with term #'x.term)
  (pattern ((~datum cons)
            (~var a (shen-prolog-term #:type-datum type-datum? #:untagged-vars untagged-vars?))
            (~var d (shen-prolog-term #:type-datum type-datum? #:untagged-vars untagged-vars?)))
           #:with term #'(cons a.term d.term))
  (pattern ((~datum #%prolog-functor)
            id:id
            (~var arg (shen-prolog-term #:type-datum type-datum? #:untagged-vars untagged-vars?)) ...+)
           #:with term #'(#%prolog-functor id arg.term ...))
  (pattern (type-sig:shen-function-type-sig)
           #:with term #'(#%prolog-functor --> type-sig.type ...))
  (pattern ((~and a:id (~not :shen-var-id))
            (~var d (shen-prolog-term #:type-datum type-datum? #:untagged-vars untagged-vars?)) ...+)
           #:with term #'(#%prolog-functor a d.term ...))
  (pattern ((~var f   (shen-prolog-term #:type-datum type-datum? #:untagged-vars untagged-vars?))
            (~var arg (shen-prolog-term #:type-datum type-datum? #:untagged-vars untagged-vars?))
            ...)
           #:with term #`(#%prolog-functor apply f.term #,(shen-cons-syntax #'(arg.term ...))))
  (pattern datum
           #:with term #'datum))

(define-splicing-syntax-class shen-cond-form
  #:attributes ((condition 1)
                (true-form 1))
  (pattern (~seq (condition:expr true-form:expr) ...+)))

(define-splicing-syntax-class shen-if-form
  (pattern (~seq condition:expr true-form:expr false-form:expr)))

(define-splicing-syntax-class shen-lambda-form
  (pattern (~seq var:shen-var-id ... body-expr:expr ...+)
           #:do [(stx-map (lambda (stx) (syntax-property stx 'bound #t))
                          #'(var ...))]))

(define-splicing-syntax-class shen-let-form
  #:attributes ((binding-id 1)
                (binding-expr 1)
                body-expr)
  (pattern (~seq binding:shen-binding ...+ body-expr:expr)
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

(define-splicing-syntax-class shen-prolog-rule
  #:attributes ((head-form 1)
                (body-form 1))
  #:datum-literals (<--)
  (pattern (~seq head-form:prolog-head-pattern ...
                 <--
                 body-form:prolog-body-pattern ...
                 (~literal |;|))))

(define-syntax-class prolog-head-pattern
  #:datum-literals (<--)
  (pattern (~not (~or <-- (~literal |;|)))))

(define-syntax-class prolog-body-pattern
  (pattern (~not (~literal |;|))))

(define-syntax-class shen-op-assoc
  (pattern (~or #:right #:left)))

(define-splicing-syntax-class macro-clause-definition
  #:attributes (body match-clause pat)
  #:datum-literals (->)
  (pattern (~seq pat:macro-clause-pattern -> body:expr)
           #:with match-clause #'[pat.pat body]))

(define-splicing-syntax-class shen-binding
  #:attributes (id expr)
  (pattern (~seq id:shen-var-id expr:expr)
           #:do [(syntax-property #'id 'bound #t)]))

(define-splicing-syntax-class function-clause-definition
  #:attributes ((pats 1)
                arrow
                body
                guard
                match-clause
                shen-prolog-body
                shen-prolog-guard
                (shen-prolog-pat 1))
  #:datum-literals (-> <- where)
  (pattern (~seq pats:function-clause-pattern ... -> body:expr
                 (~optional (~seq where guard:expr)))
           #:with arrow #'->
           #:with match-clause #'[(pats.pat ...) (~? (~@ . (#:when guard))) body]
           #:with shen-prolog-body  (syntax->shen-prolog-term #'body)
           #:with shen-prolog-guard (if (attribute guard)
                                        (syntax->shen-prolog-term #'guard)
                                        #'#t)
           #:with (shen-prolog-pat ...) (stx-map syntax->shen-prolog-term #'(pats.pat ...)))
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
                                    body)]
           #:with shen-prolog-body  (syntax->shen-prolog-term #'body)
           #:with shen-prolog-guard (if (attribute guard)
                                        (syntax->shen-prolog-term #'guard)
                                        #'#t)
           #:with (shen-prolog-pat ...) (stx-map syntax->shen-prolog-term #'(pats.pat ...))))

;; the cut in shen-define ensures a syntax error is raised if the
;; second #:fail-unless condition isn't met.
(define-splicing-syntax-class shen-define
  #:attributes (name (clause 1) (type 1) type-sig wrapper)
  #:datum-literals (|{| |}|)
  (pattern (~seq
             name:id (~optional (~seq |{| type-sig:shen-function-type-sig |}| ~!))
             clause:function-clause-definition ...+)
           #:fail-unless (apply = (map length (attribute clause.pats)))
           "each clause must contain the same number of patterns"
           #:fail-unless (if (attribute type-sig.type)
                             (= (length (first  (attribute clause.pats)))
                                (sub1   (length (attribute type-sig.type))))
                             #t)
           "the number of argument types must match the number of clause arguments"
           #:with (type ...) #'((~? (~@ type-sig.type ...) ()))
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
