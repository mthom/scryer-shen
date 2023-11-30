#lang racket

(require syntax/parse/define
         racket/stxparam
         (for-syntax racket/match racket/provide-transform syntax/parse syntax/stx))

(define-syntax-parameter fail
  (syntax-id-rules (fail) [_ 'fail]))

(define-syntax-parameter fail-if
  (syntax-id-rules (fail-if) [_ 'fail-if]))

(begin-for-syntax
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
                             (=> fail-func)
                             (unless guard (fail-func))
                             (syntax-parameterize ([fail (syntax-id-rules (fail-func)
                                                             [(fail) (fail-func)]
                                                             [fail 'fail])]
                                                   [fail-if (syntax-id-rules (fail-func)
                                                              [(fail-if e) (when e (fail-func))]
                                                              [fail-if 'fail-if])])
                               body)]))

  (define-syntax-class curry-out-export
    #:attributes (func-id renamed-id wrapper)
    (pattern [(~seq func-id:id (~optional renamed-id:id #:defaults ([renamed-id #'func-id]))
                    #:arity wrapped-arity:nat)]
      #:with wrapper #'(curry (procedure-reduce-arity func-id wrapped-arity)))
    (pattern [(~seq func-id:id renamed-id:id)]
      #:with wrapper #'(curry func-id))
    (pattern func-id:id
      #:with renamed-id #'func-id
      #:with wrapper #'(curry func-id))))

(define-syntax (top stx)
  (syntax-parse stx
    [(top . id:shen-var-id)
     #'(#%top . id)]
    [(top . id:id)
     (syntax/loc stx (quote id))]))

(define-syntax (app stx)
  (syntax-parse stx
    [(app)
     (syntax/loc stx empty)]
    [(app . form)
     (syntax/loc stx (#%app . form))]))

(define-syntax curry-out
  (make-provide-pre-transformer
   (lambda (stx modes)
     (syntax-parse stx
       [(_ f:curry-out-export ...)
        #:with (curried-f ...)
               (stx-map
               syntax-local-lift-expression
               #'(f.wrapper ...))
        (pre-expand-export
         #'(rename-out [curried-f f.renamed-id] ...)
         modes)]))))

(define-syntax-parse-rule (shen-define name:id clause:clause-definition ...+)
  #:fail-unless (apply = (map length (attribute clause.pats)))
  "each clause must contain the same number of patterns"
  #:with (arg-id ...) (generate-temporaries (car (attribute clause.pats)))
  (define name
    (curry
     (lambda (arg-id ...)
       (match* (arg-id ...)
         clause.match-clause ...)))))

(define-syntax-parse-rule (shen-let b:shen-binding ...+ body:expr)
  (let* ([b.id b.expr] ...) body))

(define-syntax-parse-rule (shen-lambda id:shen-var-id ... body:expr)
  (lambda (id ...) body))

(provide #%top-interaction
         #%datum
         (protect-out fail fail-if)
         curry-out
         (rename-out [app #%app]
                     [top #%top]
                     [shen-define define]
                     [shen-let let]
                     [shen-lambda /.]))