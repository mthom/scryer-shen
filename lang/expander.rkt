#lang racket

(require syntax/parse/define
         racket/stxparam
         (for-syntax racket/match
                     racket/provide-transform
                     syntax/parse
                     syntax/stx))

(define-values (struct:failure make-failure-object failure? failure-ref set-failure!)
  (make-struct-type 'failure #f 0 0))

(define failure-object (make-failure-object))

(define fail-if-fn
  (curry
   (lambda (fail-fn x)
     (if (fail-fn x)
         failure-object
         x))))

(define-syntax-parameter fail
  (syntax-id-rules (fail)
    [(fail) failure-object]
    [fail (lambda () failure-object)]))

(define-syntax-parameter fail-if
  (syntax-id-rules (fail)
    [(fail-if fail-fn x) (fail-if-fn fail-fn x)]
    [fail-if fail-if-fn]))

(begin-for-syntax
  (require racket/syntax)

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

  (define-syntax-class assoc
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
                    (~optional (~seq #:variadic assoc:assoc) #:defaults ([assoc #'#f])))]
      #:fail-when (and (attribute assoc) (not (= (syntax->datum (attribute wrapped-arity)) 2)))
      "variadic functions must have arity 2"
      #:with wrapper #'(curry (procedure-reduce-arity func-id wrapped-arity)))
    (pattern [(~seq func-id:id renamed-id:id)]
      #:with wrapper #'(curry func-id)
      #:with assoc #'#f)
    (pattern func-id:id
      #:with renamed-id #'func-id
      #:with wrapper #'(curry func-id)
      #:with assoc #'#f))

  (define (generate-variadic-macro-or-wrapper assoc wrapper-id)
    (case assoc
      [(#:right) (let ([new-id (generate-temporary)])
                   (syntax-local-lift-module-end-declaration
                    (with-syntax ([wrapper-id wrapper-id]
                                  [new-id new-id])
                      #'(define-syntax (new-id stx)
                          (syntax-parse stx
                            [(_ a) #'(wrapper-id a)]
                            [(_ a b) #'(wrapper-id a b)]
                            [(_ a b . cs) #'(wrapper-id a (new-id b . cs))]
                            [new-id:id #'wrapper-id]))))
                   new-id)]
      [(#:left) (let ([new-id (generate-temporary)])
                  (syntax-local-lift-module-end-declaration
                   (with-syntax ([wrapper-id wrapper-id]
                                 [new-id new-id])
                     #'(define-syntax (new-id stx)
                         (syntax-parse stx
                           [(_ a) #'(wrapper-id a)]
                           [(_ a b) #'(wrapper-id a b)]
                           [(_ a b . cs) #'(new-id (wrapper-id a b) . cs)]
                           [new-id:id #'wrapper-id]))))
                  new-id)]
      [else wrapper-id])))

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
        #:with (wrapper-f ...)
               (stx-map
                syntax-local-lift-expression
                #'(f.wrapper ...))
        #:with (curried-f ...)
               (stx-map
                (lambda (stx)
                  (match (syntax-e stx)
                    [(cons assoc wrapper-id)
                     (generate-variadic-macro-or-wrapper (syntax->datum assoc) wrapper-id)]))
                #'((f.assoc . wrapper-f) ...))
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
