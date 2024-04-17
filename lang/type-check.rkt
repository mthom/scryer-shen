#lang racket

(require data/queue
         (only-in "prolog-syntax.rkt"
                  prolog-syntax-writers)
         racket/syntax
         syntax/parse
         syntax/stx
         "syntax-utils.rkt")

(provide enqueue-type-check-queries!
         type-check?)

(define function-declare-queue    (make-queue))
(define function-type-check-queue (make-queue))
(define type-check? (make-parameter #f (lambda (val)
                                         (unless (boolean? val)
                                           (raise-type-error 'tc "boolean" val))
                                         val)))

(define (enqueue-type-check-queries! fn-name type-sig clauses)
  (define (pattern-hyps pat-types pats clause-guard)
    (with-syntax ([(pat ...) pats]
                  [(pat-type ...) pat-types])
      (shen-cons-syntax #`((#%prolog-functor type-check pat pat-type)
                           ...
                           #,@(if (eq? (syntax->datum clause-guard) #t)
                                  #'()
                                  #`((#%prolog-functor type-check
                                                       (#%prolog-functor apply #,@clause-guard)
                                                       verified)))))))

  (syntax-parse type-sig
    [(type-sig:shen-function-type-sig)
     (define-values (string-port write-prolog-goals received-vars-vec)
       (prolog-syntax-writers #t #f))

     (define declare-functor
       (with-syntax ([(type-sig-type ... result-type) #'(type-sig.type ...)])
         #`(#%prolog-functor declare #,fn-name
                              #,(foldr (lambda (type acc)
                                         #`(#%prolog-functor --> #,type #,acc))
                                       #'result-type
                                       (syntax->list #'(type-sig-type ...))))))

     (write-prolog-goals #`((retractall (#%prolog-functor : shen #,declare-functor))
                            (assertz (#%prolog-functor : shen #,declare-functor))) #t)
     (write-string ".\n" string-port)
     (printf "declare: ~a~n" (get-output-string string-port))
     (enqueue! function-declare-queue (get-output-string string-port))])

  (define shen-prolog-queries
    (syntax-parse type-sig
      [(type-sig:shen-function-type-sig)
       (with-syntax* ([(pat-type ... clause-type) #'(type-sig.type ...)]
                      [(((pat-form ...) ...)
                        (clause-body ...)
                        (clause-guard ...))
                       (syntax-parse clauses
                         [((clause:function-clause-definition) ...+)
                          #`(((clause.shen-prolog-pat ...) ...)
                             (clause.shen-prolog-body ...)
                             (clause.shen-prolog-guard ...))])]
                      [(pattern-hyp ...) (stx-map
                                          (curry pattern-hyps #'(pat-type ...))
                                          #'((pat-form ...) ...)
                                          #'(clause-guard ...))])
         #'((#%prolog-functor : shen
                              (#%prolog-functor start-proof pattern-hyp
                                                (#%prolog-functor type-check clause-body clause-type)
                                                _))
            ...))]))

  ;; #f means the query is not embedded in a Shen context
  ;; so received-vars-vec must be empty
  (define-values (string-port write-prolog-goals received-vars-vec)
    (prolog-syntax-writers #t #f))

  (write-prolog-goals shen-prolog-queries #t)
  (write-string ".\n" string-port)

  (printf "query output: ~a~n" (get-output-string string-port))

  (enqueue! function-type-check-queue (get-output-string string-port)))
