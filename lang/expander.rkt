#lang racket

(require racket/stxparam
         syntax/parse/define
         (for-syntax racket/match syntax/parse))

(define-syntax-parameter fail
  (lambda (stx)
    (raise-syntax-error #f "backtracking not defined in this clause arm" stx)))

(define-syntax-parameter fail-if
  (lambda (stx)
    (raise-syntax-error #f "backtracking not defined in this clause arm" stx)))

(begin-for-syntax
  (define (string-capitalized? string)
    (char-upper-case? (string-ref string 0)))

  (define (quote-empty-lists pattern)
    (match (syntax-e pattern)
      [(list cons-symbol hd tl)
       #:when (eq? (syntax->datum cons-symbol) 'cons)
       (with-syntax ([tl (quote-empty-lists tl)]
                     [hd hd])
         (syntax/loc pattern (cons hd tl)))]
      ['() (syntax/loc pattern (quote ()))]
      [_ pattern]))
  
  (define-syntax-class shen-var-id
    (pattern (~and id:id (~fail #:unless (string-capitalized? (symbol->string (syntax-e #'id)))))))

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
    #:datum-literals (-> <-)
    (pattern (~seq pats:clause-pattern ... -> body:expr)
      #:with match-clause #'[(pats.pat ...) body])
    (pattern (~seq pats:clause-pattern ... <- body:expr)
      #:with match-clause #'[(pats.pat ...)
                             (=> exit)
                             (let ((fail-if-fn (lambda (fail-expr)
                                                 (when fail-expr (exit)))))
                               ;; TODO: get rid of fail-if-fn somehow! but keep the fail-if syntax parameter.
                               (syntax-parameterize ([fail (make-rename-transformer #'exit)]
                                                     [fail-if (make-rename-transformer #'fail-if-fn)])                                                     
                                 body))])))

(define-syntax (top stx)
  (syntax-parse stx
    [(top . id:shen-var-id)
     (syntax/loc stx id)]
    [(top . id:id)
     (syntax/loc stx id)]))

(define-syntax (app stx)
  (syntax-parse stx
    [(app)
     (syntax/loc stx empty)]
    [(app . form)
     (syntax/loc stx (#%app . form))]))

(define-syntax-parse-rule (shen-define name:id clause:clause-definition ...+)
  #:fail-unless (apply = (map length (attribute clause.pats)))
  "each clause must contain the same number of patterns"
  #:with (arg-id ...) (generate-temporaries (car (attribute clause.pats)))
  (define/match (name arg-id ...)
    clause.match-clause ...))

(define-syntax-parse-rule (shen-let b:shen-binding ...+ body:expr)
  (let* ([b.id b.expr] ...) body))

(define-syntax-parse-rule (shen-lambda id:shen-var-id ... body:expr)
  (lambda (id ...) body))

(provide #%top-interaction
         #%datum
         (rename-out [app #%app]
                     [top #%top]
                     [shen-define define]
                     [shen-let let]                     
                     [shen-lambda /.]))