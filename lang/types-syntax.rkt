#lang racket

(require syntax/parse
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

(define-splicing-syntax-class shen-sequent-assertion
  #:attributes (assumption head-args shen-prolog-term)
  (pattern (~seq datum:shen-prolog-term (~literal :) type:shen-prolog-term)
           #:with assumption #'(#%prolog-functor : datum.term type.term)
           #:with head-args #'(datum.term type.term Hyps)
           #:with shen-prolog-term #'(shen:type-check datum.term type.term Hyps))
  (pattern (~seq goal:shen-prolog-term)
           #:with assumption #'goal.term
           #:with head-args #'(goal.term)
           #:with shen-prolog-term #'(goal.term)))

(define-splicing-syntax-class shen-sequent-assertion-list
  #:attributes ((assumption 1) (shen-prolog-term 1))
  (pattern (~seq first:shen-sequent-assertion
                 (~seq (~literal |,|) rest:shen-sequent-assertion)
                 ...)
           #:with (assumption ...) #'(first.assumption rest.assumption ...)
           #:with (shen-prolog-term ...) #'(first.shen-prolog-term rest.shen-prolog-term ...)))

(define-splicing-syntax-class shen-sequent-implicative
  #:attributes ((head-arg 1) shen-prolog-term)
  (pattern (~seq assertions:shen-sequent-assertion-list (~literal >>) conq:shen-sequent-assertion)
           #:with (head-arg ...)   #'(assertions.assumption ...)
           #:with shen-prolog-term #`(>> #,(shen-cons-syntax #'(assertions.assumption ...))
                                         conq.assumption)))

(define-splicing-syntax-class shen-sequent-condition
  #:attributes (implicative shen-prolog-terms)
  (pattern (~seq (~datum if) condition:expr)
           #:with implicative #'()
           #:with shen-prolog-terms #'((when condition)))
  (pattern (~seq (~datum let) id:shen-var-id datum:expr)
           #:with implicative #'()
           #:with shen-prolog-terms #'((is! id datum)))
  (pattern (~seq assertions:shen-sequent-assertion-list (~literal |;|))
           #:with implicative #'(assertions.assumption ...)
           #:with shen-prolog-terms #'(assertions.shen-prolog-term ...))
  (pattern (~seq implies:shen-sequent-implicative (~literal |;|))
           #:with implicative #'(implies.head-arg ...)
           #:with shen-prolog-terms #'(implies.shen-prolog-term)))

;; The consequent is the head of a type sequent being generated.
(define-splicing-syntax-class shen-sequent-consequent
  #:attributes ((head-arg 1) predicate-name shen-prolog-term)
  (pattern (~seq assertion:shen-sequent-assertion)
           #:with (head-arg ...)   #'assertion.head-args
           #:with predicate-name   #'type-check
           #:with shen-prolog-term #'assertion.shen-prolog-term)
  (pattern (~seq implies:shen-sequent-implicative)
           #:with (head-arg ...)   #'(implies.head-arg ...)
           #:with predicate-name   #'>>
           #:with shen-prolog-term #'implies.shen-prolog-term))

(define-splicing-syntax-class shen-type-sequent
  #:attributes ((prolog-form 1))
  (pattern (~seq cond:shen-sequent-condition ...
                 :shen-single-line-bar
                 conq:shen-sequent-consequent
                 (~literal |;|))
           #:with assumptions (if (free-identifier=? #'conq.predicate-name #'type-check)
                                  #'(conq.head-arg ...)
                                  #`(#,(shen-cons-syntax #'(conq.head-arg ...)) P))
           #:with (prolog-form ...) #'((defprolog conq.predicate-name
                                         (~@ . assumptions) <-- (~@ . cond.shen-prolog-terms) ... |;|)))
  (pattern (~seq cond:shen-sequent-condition ...
                 :shen-multi-line-bar
                 conq:shen-sequent-assertion
                 (~literal |;|))
           #:with conq-list (shen-cons-syntax #'(conq.assumption))
           #:with impl-list (shen-cons-syntax #'((~@ . cond.implicative) ...))
           #:with (prolog-form ...) #'((defprolog type-check
                                         (~@ . conq.head-args) <-- (~@ . cond.shen-prolog-terms) ... |;|)
                                       (defprolog >>
                                         conq-list P <-- (>> impl-list P) |;|))))
