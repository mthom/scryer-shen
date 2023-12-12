#lang racket

(require (for-template racket/base
                       racket/function
                       racket/stxparam)
         (for-meta -1 "failure.rkt")
         racket/stxparam
         syntax/parse)

(provide clause-definition
         shen-binding
         shen-var-id
         curry-out-export
         (for-meta -1 (all-from-out "failure.rkt")))

(define (capitalized-symbol? symbol)
  (and (symbol? symbol)
       (let ([string (symbol->string symbol)])
         (char-upper-case? (string-ref string 0)))))

(define (quote-empty-lists pattern)
  (match (syntax-e pattern)
    [(list cons-symbol hd tl)
     #:when (eq? (syntax->datum cons-symbol) 'cons)
     (quasisyntax/loc pattern (cons #,(quote-empty-lists hd)
                                    #,(quote-empty-lists tl)))]
    ['() (syntax/loc pattern (quote ()))]
    ['_ pattern]
    [(? (compose not capitalized-symbol?))
     (quasisyntax/loc pattern (quote #,pattern))]
    [_ pattern]))

(define-syntax-class shen-var-id
  (pattern (~and id:id (~fail #:unless (capitalized-symbol? (syntax->datum #'id))))))

(define-syntax-class clause-pattern
  #:attributes (pat)
  #:datum-literals (-> <-)
  (pattern (~and (~not ->) (~not <-))
    #:with pat (quote-empty-lists this-syntax))
  (pattern (~or (->) (<-))
    #:with pat this-syntax))

(define-syntax-class shen-op-assoc
  (pattern (~or #:right #:left)))

(define-splicing-syntax-class shen-binding
  #:attributes (id expr)
  (pattern (~seq id:shen-var-id expr:expr)))

(define-splicing-syntax-class clause-definition
  #:attributes ((pats 1) match-clause)
  #:datum-literals (-> <- where)
  (pattern (~seq pats:clause-pattern ... -> body:expr
                 (~optional (~seq where guard:expr)
                            #:defaults ([guard #'#t])))
    #:with match-clause #'[(pats.pat ...) #:when guard body])
  (pattern (~seq pats:clause-pattern ... <- body:expr
                 (~optional (~seq where guard:expr)
                            #:defaults ([guard #'#t])))
    #:with match-clause #'[(pats.pat ...)
                           (=> backtrack-fn)
                           (unless guard (backtrack-fn))
                           (syntax-parameterize ([fail (syntax-id-rules (backtrack-fn)
                                                         [(fail) (backtrack-fn)]
                                                         [fail backtrack-fn])]
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

(define-syntax-class curry-out-export
  #:attributes (func-id renamed-id wrapper assoc)
  (pattern [(~seq func-id:id (~optional renamed-id:id #:defaults ([renamed-id #'func-id]))
                  #:arity wrapped-arity:nat
                  (~optional (~seq #:polyadic assoc:shen-op-assoc) #:defaults ([assoc #'#f])))]
    #:fail-when (and (syntax->datum (attribute assoc)) (not (= (syntax->datum (attribute wrapped-arity)) 2)))
    "polyadic functions must have arity 2"
    #:with wrapper #'(curry (procedure-reduce-arity func-id wrapped-arity)))
  (pattern [(~seq func-id:id renamed-id:id)]
    #:with wrapper #'(curry func-id)
    #:with assoc #'#f)
  (pattern func-id:id
    #:with renamed-id #'func-id
    #:with wrapper #'(curry func-id)
    #:with assoc #'#f))