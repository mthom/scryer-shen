#lang racket

(require racket/stxparam
         syntax/parse/define
         (for-syntax racket/function
                     racket/match
                     racket/provide-transform
                     racket/syntax
                     syntax/parse
                     syntax/stx
                     "syntax-utils.rkt"))

(begin-for-syntax
  (define (generate-variadic-macro-or-wrapper assoc wrapper-id [new-id (generate-temporary)])
    (case assoc
      [(#:right) (syntax-local-lift-module-end-declaration
                  (with-syntax ([wrapper-id wrapper-id]
                                [new-id new-id])
                    #'(define-syntax (new-id stx)
                        (syntax-parse stx
                          [(_ a) #'(wrapper-id a)]
                          [(_ a b) #'(wrapper-id a b)]
                          [(_ a b . cs) #'(wrapper-id a (new-id b . cs))]
                          [new-id:id #'wrapper-id]))))
                 new-id]
      [(#:left) (syntax-local-lift-module-end-declaration
                 (with-syntax ([wrapper-id wrapper-id]
                               [new-id new-id])
                   #'(define-syntax (new-id stx)
                       (syntax-parse stx
                         [(_ a) #'(wrapper-id a)]
                         [(_ a b) #'(wrapper-id a b)]
                         [(_ a b . cs) #'(new-id (wrapper-id a b) . cs)]
                         [new-id:id #'wrapper-id]))))
                new-id]
      [else wrapper-id])))

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

(define-syntax (shen-true stx)
  (syntax-case stx ()
    [_:id #'#t]))

(define-syntax (shen-false stx)
  (syntax-case stx ()
    [_:id #'#f]))

(define shen-variable-namespace
  (make-empty-namespace))

(provide #%top-interaction
         #%datum
         curry-out
         shen-variable-namespace
         (rename-out [app #%app]
                     [top #%top]
                     [shen-true true]
                     [shen-false false]
                     [shen-define define]
                     [shen-let let]
                     [shen-lambda /.]))
