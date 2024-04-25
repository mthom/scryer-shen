#lang racket

(require data/gvector
         (for-template
          racket/base
          racket/match
          racket/stxparam
          "failure.rkt"
          (only-in racket/function
                   curry))
         "pairs.rkt"
         racket
         racket/generator
         racket/match
         racket/syntax
         syntax/parse
         syntax/parse/define
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
         syntax->shen-prolog-term
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
    [(~and id:id (~not :shen-var-id))
     #''id]
    [_ pattern]))

(define (pattern-variables pattern)
  (for/list ([var (in-generator
                   (let loop ([pattern pattern])
                     (syntax-parse pattern
                       [(a . d)
                        (loop #'a)
                        (loop #'d)]
                       [:shen-var-id
                        (yield pattern)]
                       [_ (void)])))])
    var))

(define-literal-set shen-special-forms
  #:datum-literals (true false let cons if /. cond fn @p @s @v)
  ())

(define shen-special-form?
  (literal-set->predicate shen-special-forms))

(define-syntax-class unit-string
  (pattern str:string
           #:fail-unless (= (string-length (syntax-e #'str)) 1)
           "string literal patterns must consist of a single character"))

(define (syntax->shen-prolog-term stx [type-datum? #t] [untagged-vars? #f])
  (syntax-parse stx
    [(~var term (shen-prolog-term #:type-datum type-datum?
                                  #:untagged-vars untagged-vars?))
     #'term.term]))

(define shen-cons-syntax
  (syntax-parser
    [(a . d)
     #`(cons a #,(shen-cons-syntax #'d))]
    [stx
     #'stx]))

(define-splicing-syntax-class (shen-function-type-sig #:type-datum [type-datum? #f]
                                                      #:untagged-vars [untagged-vars? #t])
  #:attributes ((type 1))
  #:datum-literals (-->)
  (pattern (~seq (~var t1 (shen-prolog-term #:type-datum type-datum?
                                            #:untagged-vars untagged-vars?))
                 (~seq --> (~var t2 (shen-prolog-term #:type-datum type-datum?
                                                      #:untagged-vars untagged-vars?))) ...+)
           #:with (type ...) #'(t1.term t2.term ...))
  (pattern (~seq --> (~var t (shen-prolog-term #:type-datum type-datum?
                                               #:untagged-vars untagged-vars?)))
           #:with (type ...) #'(t.term)))

(define local-pattern-variables (make-parameter #'()))

(define-syntax-parse-rule (with-local-pattern-variable var body ...+)
  (parameterize ([local-pattern-variables #`(#,var . #,(local-pattern-variables))])
    body ...))

(define (wrap-tagged-shen-prolog-term datum-term untagged-vars?)
  (define wrapper (syntax-parse-state-ref (syntax->datum datum-term) #f))
  (cond [(procedure? wrapper)
         (wrapper datum-term)]
        [(not untagged-vars?)
         (define datum (syntax->datum datum-term))
         #`(#%prolog-functor
            #,(if (memf (lambda (var) (equal? datum (syntax->datum var)))
                        (syntax->list (local-pattern-variables)))
                  #'?
                  #'symbol)
            #,datum-term)]
        [else datum-term]))

(define (syntax->shen-function-term stx [type-datum? #t] [untagged-vars? #f])
  (syntax-parse stx
    [(~and id:id (~not :shen-var-id))
     #'(#%prolog-functor fn id)]
    [(~var term (shen-prolog-term #:type-datum type-datum?
                                  #:untagged-vars untagged-vars?))
     #'term.term]))

(define-syntax-class (shen-prolog-term #:type-datum [type-datum? #t]
                                       #:untagged-vars [untagged-vars? #f])
  #:attributes (term)
  (pattern (~literal <>)
           #:with term #'<>)
  (pattern str:string
           #:with term (if type-datum?
                           #'(#%prolog-functor string str)
                           #'str))
  (pattern num:number
           #:with term (if type-datum?
                           #'(#%prolog-functor number num)
                           #'num))
  (pattern sym:shen-var-id
           #:with term (if type-datum?
                           (wrap-tagged-shen-prolog-term #'sym untagged-vars?)
                           #'sym))
  (pattern (~and sym:id (~not :shen-var-id))
           #:fail-when (or (shen-special-form? #'sym)
                           (syntax-parse #'sym
                             [(~or (~literal #%prolog-functor)
                                   (~literal apply))
                              #t]
                             [_ #f]))
           #f
           #:with term (if type-datum? #'(#%prolog-functor symbol sym) #'sym))
  (pattern ((~datum fn) arg:id)
           #:with term #'(#%prolog-functor fn arg))
  (pattern ((~and id:id (~or (~datum @p)
                             (~datum @s)
                             (~datum @v)))
            (~var first-arg (shen-prolog-term #:type-datum type-datum?
                                              #:untagged-vars untagged-vars?))
            (~var second-arg (shen-prolog-term #:type-datum type-datum?
                                               #:untagged-vars untagged-vars?))
            args ...)
           #:with term (if (stx-null? #'(args ...))
                           #'(#%prolog-functor id first-arg.term second-arg.term)
                           #`(#%prolog-functor id
                                               first-arg.term
                                               #,(syntax->shen-prolog-term
                                                  #'(id second-arg args ...)
                                                  type-datum?
                                                  untagged-vars?))))
  (pattern ((~datum let) first-b:shen-binding remaining-b ... body-expr:expr)
           #:with id-shen-prolog-term (if (and type-datum? (not untagged-vars?))
                                          #'(#%prolog-functor ? first-b.id)
                                          (syntax->shen-prolog-term #'first-b.id
                                                                    type-datum?
                                                                    untagged-vars?))
           #:with expr-shen-prolog-term (syntax->shen-prolog-term #'first-b.expr
                                                                  type-datum?
                                                                  untagged-vars?)
           #:with term (with-local-pattern-variable #'first-b.id
                         #`(#%prolog-functor let id-shen-prolog-term expr-shen-prolog-term
                                             #,(syntax->shen-prolog-term
                                                (if (stx-pair? #'(remaining-b ...))
                                                    #'(let remaining-b ... body-expr)
                                                    #'body-expr)
                                                type-datum?
                                                untagged-vars?))))
  (pattern ((~datum /.) var:shen-var-id remaining-var:shen-var-id ... body-expr:expr)
           #:with id-shen-prolog-term (if (and type-datum? (not untagged-vars?))
                                          #'(#%prolog-functor ? var)
                                          (syntax->shen-prolog-term #'var
                                                                    type-datum?
                                                                    untagged-vars?))
           #:with expr-shen-prolog-term (with-local-pattern-variable #'var
                                          (syntax->shen-prolog-term
                                           (if (stx-pair? #'(remaining-var ...))
                                               #'(/. remaining-var ... body-expr)
                                               #'body-expr)
                                           type-datum?
                                           untagged-vars?))
           #:with term #'(#%prolog-functor /. id-shen-prolog-term expr-shen-prolog-term))
  ;; strip off quote installed by quote-patterns etc.
  (pattern ((~literal quote)
            (~var x (shen-prolog-term #:type-datum type-datum?
                                      #:untagged-vars untagged-vars?)))
           #:with term #'x.term)
  (pattern ((~datum cons)
            (~var a (shen-prolog-term #:type-datum type-datum?
                                      #:untagged-vars untagged-vars?))
            (~var d (shen-prolog-term #:type-datum type-datum?
                                      #:untagged-vars untagged-vars?)))
           #:with term #'(cons a.term d.term))
  (pattern ((~datum #%prolog-functor)
            id:id
            (~var arg (shen-prolog-term #:type-datum type-datum?
                                        #:untagged-vars untagged-vars?))
            ...+)
           #:with term #'(#%prolog-functor id arg.term ...))
  (pattern ((~var type-sig (shen-function-type-sig #:type-datum type-datum?
                                                   #:untagged-vars untagged-vars?)))
           #:with term #'(#%prolog-functor --> type-sig.type ...))
  (pattern ((~var f             (shen-prolog-term #:type-datum type-datum?
                                                  #:untagged-vars untagged-vars?))
            (~var first-arg     (shen-prolog-term #:type-datum type-datum?
                                                  #:untagged-vars untagged-vars?))
            (~var remaining-arg (shen-prolog-term #:type-datum type-datum?
                                                  #:untagged-vars untagged-vars?))
            ...)
           #:when type-datum?
           #:with fn-term        (syntax->shen-function-term #'f
                                                             type-datum?
                                                             untagged-vars?)
           #:with inner-apply    #'(#%prolog-functor apply fn-term first-arg.term)
           #:with (arg-term ...) (stx-map (lambda (stx)
                                            (syntax->shen-prolog-term
                                             stx
                                             type-datum?
                                             untagged-vars?))
                                          #'(remaining-arg ...))
           #:with term           (foldl (lambda (arg-term acc)
                                          #`(#%prolog-functor apply #,acc #,arg-term))
                                        #'inner-apply
                                        (syntax->list #'(arg-term ...))))
  (pattern ((~and a:id (~not :shen-var-id))
            (~var d (shen-prolog-term #:type-datum type-datum?
                                      #:untagged-vars untagged-vars?))
            ...+)
           #:with term #'(#%prolog-functor a d.term ...))
  (pattern datum
           #:with term #'datum))

(define-splicing-syntax-class shen-cond-form
  #:attributes ((condition 1)
                (true-form 1))
  (pattern (~seq (condition:expr true-form:expr) ...+)))

(define-splicing-syntax-class shen-if-form
  (pattern (~seq condition:expr true-form:expr false-form:expr)))

(define-splicing-syntax-class shen-lambda-form
  (pattern (~seq var:shen-var-id ... body-expr:expr)
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
  #:attributes (pat (vars 1))
  #:datum-literals (-> <-)
  (pattern (~and (~not ->) (~not <-))
           #:with pat (quote-pattern this-syntax)
           #:with (vars ...) (pattern-variables this-syntax))
  (pattern (~or (->) (<-))
           #:with pat this-syntax
           #:with (vars ...) (pattern-variables this-syntax)))

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
           #:do [(syntax-property #'id 'bound #t)]
           #:with id-shen-prolog-term   (syntax->shen-prolog-term #'id)
           #:with expr-shen-prolog-term (syntax->shen-prolog-term #'expr)))

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
           #:with pat-vars #'(pats.vars ... ...)
           #:with match-clause #'[(pats.pat ...) (~? (~@ . (#:when guard))) body]
           #:with shen-prolog-body  (parameterize ([local-pattern-variables #'pat-vars])
                                      (syntax->shen-prolog-term #'body))
           #:with shen-prolog-guard (parameterize ([local-pattern-variables #'pat-vars])
                                      (if (attribute guard)
                                          (syntax->shen-prolog-term #'guard)
                                          #'#t))
           #:with (shen-prolog-pat ...) (parameterize ([local-pattern-variables #'pat-vars])
                                          (stx-map syntax->shen-prolog-term #'(pats.pat ...))))
  (pattern (~seq pats:function-clause-pattern ... <- body:expr
                 (~optional (~seq where guard:expr)))
           #:with arrow #'<-
           #:with pat-vars #'(pats.vars ... ...)
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
           #:with shen-prolog-body  (parameterize ([local-pattern-variables #'pat-vars])
                                      (syntax->shen-prolog-term #'body))
           #:with shen-prolog-guard (parameterize ([local-pattern-variables #'pat-vars])
                                      (if (attribute guard)
                                          (syntax->shen-prolog-term #'guard)
                                          #'#t))
           #:with (shen-prolog-pat ...) (parameterize ([local-pattern-variables #'pat-vars])
                                          (stx-map syntax->shen-prolog-term #'(pats.pat ...)))))

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
