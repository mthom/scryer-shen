#lang racket

(require racket/stxparam
         syntax/parse/define
         "syntax-utils.rkt"
         (for-syntax racket/base
                     racket/function
                     racket/match
                     racket/provide-transform
                     racket/syntax
                     syntax/id-table
                     syntax/parse
                     syntax/stx))

(define shen-variable-bindings
  (make-hasheq))

(define shen-function-bindings
  (make-hasheq))

(begin-for-syntax
  (define (generate-variadic-macro-or-wrapper assoc wrapper-id [new-id (generate-temporary "wrapper-macro")])
    (case assoc
      [(#:right) (with-syntax ([wrapper-id wrapper-id]
                               [new-id new-id])
                   (syntax-local-lift-module-end-declaration
                    #'(define-syntax new-id
                        (syntax-parser
                           [(_ a) #'(wrapper-id a)]
                           [(_ a b) #'(wrapper-id a b)]
                           [(_ a b . cs) #'(wrapper-id a (new-id b . cs))]))))
                 new-id]
      [(#:left) (with-syntax ([wrapper-id wrapper-id]
                              [new-id new-id])
                  (syntax-local-lift-module-end-declaration
                   #'(define-syntax new-id
                        (syntax-parser
                           [(_ a) #'(wrapper-id a)]
                           [(_ a b) #'(wrapper-id a b)]
                           [(_ a b . cs) #'(wrapper-id a (new-id b . cs))]))))
                new-id]
      [else wrapper-id])))

(define-syntax define-shen-function
  (syntax-parser
    [(_ (fn:id args ...) body:expr)
     #:with spaced-fn ((make-interned-syntax-introducer 'function) #'fn)
     #'(define (spaced-fn args ...) body)]
    [(_ fn:id racket-fn:id)
     #:with spaced-fn ((make-interned-syntax-introducer 'function) #'fn)
     #'(define spaced-fn racket-fn)]))

(define-syntax curry-out
  (make-provide-pre-transformer
   (lambda (stx modes)
     (syntax-parse stx
       [(_ f:curry-out-export ...)
        #:with (wrapper-f ...)
               (stx-map
                (lambda (stx)
                  (let* ([old-wrapper-id (generate-temporary "wrapper")]
                         [new-wrapper-id ((make-interned-syntax-introducer 'function) old-wrapper-id)])
                    (with-syntax ([wrapper-id new-wrapper-id]
                                  [stx stx])
                      (syntax-local-lift-module-end-declaration
                       #'(define wrapper-id stx)))
                    new-wrapper-id))
                #'(f.wrapper ...))
        #:with (exports ...)
               (stx-map
                (lambda (stx)
                  (match (syntax-e stx)
                    [(list assoc renamed-id curry-wrapper wrapper-id)
                     (with-syntax ([macro-or-wrapper-id (generate-variadic-macro-or-wrapper (syntax->datum assoc) wrapper-id)]
                                   [curry-wrapper curry-wrapper]
                                   [renamed-id renamed-id])
                       (syntax-local-lift-expression
                        #'(hash-set! shen-function-bindings 'renamed-id curry-wrapper))
                       (if (syntax->datum assoc)
                           #'(rename-out [macro-or-wrapper-id renamed-id])
                           #'(for-space function (rename-out [macro-or-wrapper-id renamed-id]))))]))
                #'((f.assoc f.renamed-id f.wrapper wrapper-f) ...))
        (pre-expand-export
         #'(combine-out exports ...)
         modes)]))))

(define-syntax shen-function-out
  (make-provide-pre-transformer
   (lambda (stx modes)
     (syntax-parse stx
       [(_ f:shen-function-out-export ...)
        #:do [(stx-map
               syntax-local-lift-module-end-declaration
               #'((define-shen-function f.renamed-id f.func-id) ...))]
        #:do [(stx-map
               syntax-local-lift-expression
               #'((hash-set! shen-function-bindings 'f.renamed-id f.func-id) ...))]
        (pre-expand-export
         #'(for-space function f.renamed-id ...)
         modes)]))))

(define-syntax-parse-rule (shen-define name:id clause:clause-definition ...+)
  #:fail-unless (apply = (map length (attribute clause.pats)))
  "each clause must contain the same number of patterns"
  #:with (arg-id ...) (generate-temporaries (car (attribute clause.pats)))
  #:with interned-name ((make-interned-syntax-introducer 'function) #'name)
  (define interned-name
    (curry
     (lambda (arg-id ...)
       (match* (arg-id ...)
         clause.match-clause ...)))))

(define-syntax-parse-rule (shen-let b:shen-binding ...+ body:expr)
  (let* ([b.id b.expr] ...) body))

(define-syntax-parse-rule (shen-lambda id:shen-var-id ... body:expr)
  (lambda (id ...) body))

(define-syntax (shen-true stx)
  (syntax-case stx ()
    [_:id #'#t]))

(define-syntax (shen-false stx)
  (syntax-case stx ()
    [_:id #'#f]))

(provide curry-out
         shen-function-out
         define-shen-function
         shen-function-bindings
         shen-variable-bindings
         (rename-out [shen-true true]
                     [shen-false false]
                     [shen-define define]
                     [shen-let let]
                     [shen-lambda /.]))
