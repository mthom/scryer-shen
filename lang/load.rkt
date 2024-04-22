#lang racket

(require "macros.rkt"
         "prolog.rkt"
         (only-in "reader.rkt"
                  [read-syntax shen:read-syntax])
         "prolog-syntax-expanders.rkt"
         syntax/strip-context
         "syntax-utils.rkt"
         "type-syntax-expanders.rkt")

(provide (protect-out (struct-out shen-type-check-exn))
         expression-type-check
         load
         post-load-type-check!)

(struct shen-type-check-exn exn:fail ()
  #:transparent)

;; return #t iff it was passed an expression
(define (post-load-type-check!)
  (define-values (datatype-definitions assert-declares retract-declares clause-queries)
    (type-check-definitions-and-queries))

  (clear-type-check-queues!)

  (unless (and (empty? datatype-definitions)
               (empty? assert-declares)
               (empty? retract-declares)
               (empty? clause-queries))
    (with-handlers ([exn:fail? (lambda (e)
                                 (for ([retract-declare (in-list retract-declares)])
                                   (run-prolog-query! retract-declare))
                                 (raise e))])
      (for ([datatype-definition (in-list datatype-definitions)])
        (add-prolog-predicate! datatype-definition))

      (for ([assert-declare (in-list assert-declares)])
        (run-prolog-query! assert-declare))

      (for ([clause-query (in-list clause-queries)])
        (unless (run-prolog-query! clause-query)
          (raise (shen-type-check-exn "failed" (current-continuation-marks))))))))

(define (expression-type-check pre-eval-syntax)
  (define type-query
    #`((#%prolog-functor : type-checker
                         (#%prolog-functor start-proof []
                                           (#%prolog-functor type-check #,pre-eval-syntax
                                                             ResultType)
                                           _))
       (type-check-return ResultType)))

  (define query-string (eval-syntax (expand-shen-prolog-query type-query)))
  (define query-result (run-prolog-query! query-string))

  (or query-result (raise (shen-type-check-exn "failed" (current-continuation-marks)))))

(define (load filename)
  (define in (open-input-file filename))
  (define expanded-forms
    ;; (parameterize ([current-readtable shen-readtable])
    (for/list ([stx (in-port (curry shen:read-syntax (object-name in)) in)])
      (expand (strip-context (expand-shen-form stx)))))
  (close-input-port in)
  (eval-syntax #`(begin
                   #,@expanded-forms
                   (post-load-type-check!)))
  'loaded)
