#lang racket

(require data/queue
         (for-syntax "prolog-syntax.rkt"
                     "prolog-syntax-expanders.rkt"
                     racket
                     racket/syntax
                     syntax/parse
                     syntax/stx
                     "syntax-utils.rkt"
                     "types-syntax.rkt"))

(provide (for-syntax function-def->type-check-queries
                     datatype->type-definition)
         clear-type-check-queues!
         enqueue-datatype-definition!
         enqueue-function-type-data!
         type-check?
         type-check-definitions-and-queries)

(define type-check? (make-parameter #f (lambda (val)
                                         (unless (boolean? val)
                                           (raise-type-error 'tc "boolean" val))
                                         val)))

(define function-assert-declare-queue (make-queue))
(define function-retract-declare-queue (make-queue))
(define function-type-check-queue (make-queue))
(define datatype-definition-queue (make-queue))

(define (clear-type-check-queues!)
  (set! datatype-definition-queue (make-queue))
  (set! function-assert-declare-queue (make-queue))
  (set! function-retract-declare-queue (make-queue))
  (set! function-type-check-queue (make-queue)))

(define (type-check-definitions-and-queries)
  (values (queue->list datatype-definition-queue)
          (queue->list function-assert-declare-queue)
          (queue->list function-retract-declare-queue)
          (queue->list function-type-check-queue)))

(define (enqueue-datatype-definition! type-definition)
  (enqueue! datatype-definition-queue type-definition))

(define (enqueue-function-type-data! query-strings assert-string retract-string)
  (for ([query-string (in-list query-strings)])
    (enqueue! function-type-check-queue query-string))

  (enqueue! function-assert-declare-queue assert-string)
  (enqueue! function-retract-declare-queue retract-string))

(begin-for-syntax
  (define (datatype->type-definition type-module-name sequents)
    (syntax-parse sequents
      [((sequent:shen-type-sequent) ...+)
       (apply string-append
              (format ":- module('~a#type', []).\n\n"
                      (syntax->datum type-module-name))
              (stx-map (lambda (stx)
                         (syntax-parse stx
                           [((~datum defprolog) rule-name:id rule:shen-prolog-rule ...+)
                            (expand-shen-defprolog #'rule-name #'(rule ...))]))
                       #'(sequent.prolog-form ... ...)))]))

  (define (function-def->type-check-queries fn-name type-sig clauses)
    (define (pattern-hyps pat-types clause-strings pats clause-guard)
      (with-syntax ([(pat ...) pats]
                    [(pat-type ...) pat-types]
                    [(clause-string ...) clause-strings])
        (shen-cons-syntax #`((#%prolog-functor type_check pat pat-type)
                             ...
                             (#%prolog-functor type_check clause-string string)
                             ...
                             #,@(if (eq? (syntax->datum clause-guard) #t)
                                    #'()
                                    #`((#%prolog-functor type_check
                                                         #,clause-guard
                                                         verified)))))))

    (define-values (assert-declare-string retract-declare-string)
      (syntax-parse type-sig
        [(type-sig:shen-function-type-sig)
         (define declare-functor
          (with-syntax ([(type-sig-type ... result-type) #'(type-sig.type ...)])
            #`(#%prolog-functor declare #,fn-name
                                #,(if (stx-null? #'(type-sig-type ...))
                                      #'(#%prolog-functor --> result-type)
                                      (foldr (lambda (type acc)
                                               #`(#%prolog-functor --> #,type #,acc))
                                             #'result-type
                                             (syntax->list #'(type-sig-type ...)))))))

         (values
          (let-values ([(string-port write-prolog-goals received-vars-vec) (prolog-syntax-writers #t #f)])
            ;; assert the function declares
            (write-prolog-goals #`((retractall (#%prolog-functor : inference_rules #,declare-functor))
                                   (assertz (#%prolog-functor : inference_rules #,declare-functor)))
                                #t)
            (get-output-string string-port))

          (let-values ([(string-port write-prolog-goals received-vars-vec) (prolog-syntax-writers #t #f)])
            ;; retract the function declares if type checking fails
            (define-values (string-port write-prolog-goals received-vars-vec)
              (prolog-syntax-writers #t #f))

            (write-prolog-goals #`((retractall (#%prolog-functor : inference_rules #,declare-functor))) #t)
            (get-output-string string-port)))]))

    (define shen-prolog-queries
      (syntax-parse type-sig
        [(type-sig:shen-function-type-sig)
         (with-syntax* ([(pat-type ... clause-type) #'(type-sig.type ...)]
                        [(((pat-form ...) ...)
                          (clause-body ...)
                          (clause-guard ...)
                          ((clause-string ...) ...))
                         (syntax-parse clauses
                           [((clause:function-clause-definition) ...+)
                            #'(((clause.shen-prolog-pat ...) ...)
                               (clause.shen-prolog-body ...)
                               (clause.shen-prolog-guard ...)
                               ((clause.shen-string ...) ...))])]
                        [(pattern-hyp ...) (stx-map
                                            (curry pattern-hyps #'(pat-type ...))
                                            #'((clause-string ...) ...)
                                            #'((pat-form ...) ...)
                                            #'(clause-guard ...))])
           #'(((: type_checker (#%prolog-functor start_proof
                                 pattern-hyp
                                 (#%prolog-functor type_check clause-body clause-type)
                                 _)))
              ...))]))

    (values (for/list ([query-syntax (in-syntax shen-prolog-queries)])
              (define-values (string-port write-prolog-goals received-vars-vec)
                (prolog-syntax-writers #t #f))
              (write-prolog-goals query-syntax #t)
              (get-output-string string-port))
            assert-declare-string
            retract-declare-string)))
