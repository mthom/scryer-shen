#lang racket

(require syntax/parse
         syntax/stx
         "syntax-utils.rkt")

(provide shen-type-sequent)

(define-syntax-class shen-single-line-bar
  (pattern :id
           #:fail-unless (regexp-match #rx"^_+$"
                                       (symbol->string (syntax-e this-syntax)))
           #f))

(define-syntax-class shen-multi-line-bar
  (pattern :id
           #:fail-unless (regexp-match #rx"^=+$"
                                       (symbol->string (syntax-e this-syntax)))
           #f))

(define (tag-functor-syntax! stx functor-tag)
  (let ([datum (syntax->datum stx)])
    (syntax-parse-state-set! datum (lambda (stx) #`(#%prolog-functor #,functor-tag #,stx)))
    (syntax-parse-state-cons! 'tagged-type-data datum empty)))

(define (reset-tagged-syntax-parse-state!)
  (for ([key (in-list (syntax-parse-state-ref 'tagged-type-data empty))])
    (syntax-parse-state-set! key #f))
  (syntax-parse-state-set! 'tagged-type-data empty))

(define-splicing-syntax-class shen-type-declaration
  #:attributes (datum type)
  (pattern (~seq num (~literal :) (~literal number))
           #:do [(tag-functor-syntax! #'num #'number)]
           #:with var-term (syntax->shen-prolog-term #'var #t #t)
           #:with datum #'(#%prolog-functor #%number var-term)
           #:with type  #'number)
  (pattern (~seq str (~literal :) (~literal string))
           #:do [(tag-functor-syntax! #'str #'string)]
           #:with str-term (syntax->shen-prolog-term #'str #t #t)
           #:with datum #'(#%prolog-functor #%string str-term)
           #:with type  #'string)
  (pattern (~seq sym (~literal :) (~literal symbol))
           #:do [(tag-functor-syntax! #'sym #'symbol)]
           #:with sym-term (syntax->shen-prolog-term #'sym #t #t)
           #:with datum #'(#%prolog-functor #%symbol sym-term)
           #:with type  #'symbol)
  (pattern (~seq datum-term (~literal :) type-term)
           #:with datum (syntax->shen-prolog-term #'datum-term #t #t)
           #:with type  (syntax->shen-prolog-term #'type-term #f)))

(define-splicing-syntax-class shen-type-equation
  #:attributes (first-arg second-arg)
  (pattern (~seq first-arg-term (~literal ~) second-arg-term)
           #:with first-arg  (syntax->shen-prolog-term #'first-arg-term  #f #t)
           #:with second-arg (syntax->shen-prolog-term #'second-arg-term #f #t)))

(define-splicing-syntax-class shen-sequent-assertion
  #:attributes (assumption head-args shen-prolog-term)
  (pattern (~seq type-decl:shen-type-declaration)
           #:with assumption #'(#%prolog-functor type_check type-decl.datum type-decl.type)
           #:with head-args  #'(type-decl.datum type-decl.type)
           #:with shen-prolog-term #'(#%prolog-functor type_check type-decl.datum type-decl.type))
  (pattern (~seq type-equation:shen-type-equation)
           #:with assumption #'(#%prolog-functor type_eq type-equation.first-arg type-equation.second-arg)
           #:with head-args  #'(type-equation.first-arg type-equation.second-arg)
           #:with shen-prolog-term #'(#%prolog-functor g (#%prolog-functor type_eq
                                                                           type-equation.first-arg
                                                                           type-equation.second-arg)))
  (pattern goal
           #:with goal-term (syntax->shen-prolog-term #'goal #f #t)
           #:with assumption #'(#%prolog-functor : user goal-term)
           #:with head-args  #'goal-term
           #:with shen-prolog-term #'(#%prolog-functor g (#%prolog-functor : user goal-term))))

(define-splicing-syntax-class shen-sequent-assertion-list
  #:attributes ((assumption 1) (shen-prolog-term 1))
  (pattern (~seq first:shen-sequent-assertion
                 (~seq (~literal |,|) rest:shen-sequent-assertion)
                 ...)
           #:with (assumption ...) #'(first.assumption rest.assumption ...)
           #:with (shen-prolog-term ...) #'(first.shen-prolog-term rest.shen-prolog-term ...)))

(define-splicing-syntax-class shen-sequent-condition
  #:attributes (implicative shen-prolog-terms)
  (pattern (~seq (~datum if) condition-term)
           #:cut
           #:with condition (syntax->shen-prolog-term #'condition-term #f #t)
           #:with implicative #'()
           #:with shen-prolog-terms #'((#%prolog-functor g (#%prolog-functor shen_if_condition
                                                                             condition))))
  (pattern (~seq (~datum let) id:shen-var-id datum:expr)
           #:cut
           #:with implicative #'()
           #:with shen-prolog-terms #'((is! id datum)))
  (pattern (~seq assertions:shen-sequent-assertion-list (~literal |;|))
           #:with implicative #'(assertions.assumption ...)
           #:with shen-prolog-terms #'(assertions.shen-prolog-term ...))
  (pattern (~seq implies:shen-sequent-implicative (~literal |;|))
           #:with implicative #'(implies.head-arg ...)
           #:with shen-prolog-terms #'(implies.shen-prolog-term)))

(define-splicing-syntax-class shen-sequent-implicative
  #:attributes (conq (head-arg 1) shen-prolog-term)
  (pattern (~seq assertions:shen-sequent-assertion-list (~literal >>) ps:shen-sequent-assertion-list)
           #:with conq             (if (stx-null? (stx-cdr #'(ps.assumption ...)))
                                       (stx-car #'(ps.assumption ...))
                                       (shen-cons-syntax #'(ps.assumption ...)))
           #:with (head-arg ...)   #'(assertions.assumption ...)
           #:with shen-prolog-term #`(>> #,(shen-cons-syntax #'(assertions.assumption ...))
                                         conq)))

;; The consequent is the head of a type sequent being generated.
(define-splicing-syntax-class shen-sequent-consequent
  #:attributes (assumptions predicate-name shen-prolog-term)
  (pattern (~seq assertion:shen-sequent-assertion)
           #:with assumptions      #'assertion.head-args
           #:with predicate-name   #'type_check
           #:with shen-prolog-term #'assertion.shen-prolog-term)
  (pattern (~seq implies:shen-sequent-implicative)
           #:with assumptions      #`(#,(shen-cons-syntax #'(implies.head-arg ...)) implies.conq)
           #:with predicate-name   #'>>
           #:with shen-prolog-term #'implies.shen-prolog-term))

(define-splicing-syntax-class shen-type-sequent
  #:attributes ((prolog-form 1))
  (pattern (~seq cond:shen-sequent-condition ...
                 :shen-single-line-bar
                 conq:shen-sequent-consequent
                 (~literal |;|))
           #:do [(reset-tagged-syntax-parse-state!)]
           #:with (prolog-form ...) #'((defprolog conq.predicate-name
                                         (~@ . conq.assumptions) <--
                                         (~@ . cond.shen-prolog-terms) ... |;|)))
  (pattern (~seq cond:shen-sequent-condition ...
                 :shen-multi-line-bar
                 conq:shen-sequent-assertion
                 (~literal |;|))
           #:do [(reset-tagged-syntax-parse-state!)]
           #:with conq-list (shen-cons-syntax #'(conq.assumption))
           #:with impl-list (shen-cons-syntax #'((~@ . cond.implicative) ...))
           #:with (prolog-form ...) #'((defprolog type_check
                                         (~@ . conq.head-args) <-- (~@ . cond.shen-prolog-terms) ... |;|)
                                       (defprolog >>
                                         conq-list P <-- (>> impl-list P) |;|))))
