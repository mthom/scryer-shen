#lang racket

(require (for-syntax data/queue
                     (only-in "prolog-syntax.rkt"
                              prolog-syntax-writers)
                     racket
                     racket/syntax
                     syntax/parse
                     syntax/stx
                     "syntax-utils.rkt"))

(provide (for-syntax assert-functor-declare-functor-queue!
                     clear-functor-declare-functor-queue!
                     enqueue-type-check-queries!
                     retract-functor-declare-functor-queue!)
         type-check?)

(define type-check? (make-parameter #f (lambda (val)
                                         (unless (boolean? val)
                                           (raise-type-error 'tc "boolean" val))
                                         val)))

(begin-for-syntax
  (define function-declare-functor-queue (make-queue))
  (define function-type-check-queue (make-queue))

  (define (assert-functor-declare-functor-queue!)
    (for/list ([declare-functor (in-queue function-declare-functor-queue)])
      (define-values (string-port write-prolog-goals received-vars-vec)
        (prolog-syntax-writers #t #f))

      (write-prolog-goals #`((retractall (#%prolog-functor : inference-rules #,declare-functor))
                             (assertz (#%prolog-functor : inference-rules #,declare-functor)))
                          #t)
      (write-string ".\n" string-port)

      (get-output-string string-port)))

  (define (retract-functor-declare-functor-queue!)
    (for/list ([declare-functor (in-queue function-declare-functor-queue)])
      (define-values (string-port write-prolog-goals received-vars-vec)
        (prolog-syntax-writers #t #f))

      (write-prolog-goals #`((retractall (#%prolog-functor : inference-rules #,declare-functor)))
                          #t)
      (write-string ".\n" string-port)

      (get-output-string string-port)))

  (define (clear-functor-declare-functor-queue!)
    (set! function-declare-functor-queue (make-queue)))

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
       (define declare-functor
         (with-syntax ([(type-sig-type ... result-type) #'(type-sig.type ...)])
           #`(#%prolog-functor declare #,fn-name
                               #,(foldr (lambda (type acc)
                                          #`(#%prolog-functor --> #,type #,acc))
                                        #'result-type
                                        (syntax->list #'(type-sig-type ...))))))

       (printf "declare: ~a~n" declare-functor)
       (enqueue! function-declare-functor-queue declare-functor)])

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
                                (#%prolog-functor start-proof
                                                  pattern-hyp
                                                  (#%prolog-functor type-check clause-body clause-type)
                                                  _))
              ...))]))

    (for ([query-syntax (in-syntax shen-prolog-queries)])
      (define-values (string-port write-prolog-goals received-vars-vec)
        (prolog-syntax-writers #t #f))

      (write-prolog-goals query-syntax #t)
      (write-string ".\n" string-port)

      (printf "query output: ~a~n" (get-output-string string-port))
      (enqueue! function-type-check-queue (get-output-string string-port)))))
