#lang racket

(require (only-in "expander.rkt"
                  shen-function-out)
         "macros.rkt"
         "namespaces.rkt"
         "printer.rkt"
         "prolog.rkt"
         "prolog-syntax-expanders.rkt"
         (only-in "reader.rkt"
                  detect-prolog-syntax
                  shen-readtable)
         syntax/parse
         "syntax-utils.rkt"
         (only-in "system-functions.rkt"
                  [eval shen:eval])
         "type-syntax-expanders.rkt")

(provide (protect-out (struct-out shen-type-check-exn))
         (shen-function-out load)
         expression-type-check
         load-shen-form
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
    #`((#%prolog-functor : type_checker
                         (#%prolog-functor start_proof []
                                           (#%prolog-functor type_check
                                                             #,pre-eval-syntax
                                                             ResultType)
                                           _))
       (type-check-return ResultType)))

  (define query-string (eval-syntax (expand-shen-prolog-query type-query)))
  (define query-result (run-prolog-query! query-string))

  (or query-result
      (raise (shen-type-check-exn "failed" (current-continuation-marks)))))

(define (load-shen-form pre-eval-stx)
  (syntax-parse pre-eval-stx
    [((~literal define) name:id . _)
     (let ([result (shen:eval (syntax->datum pre-eval-stx))])
       (if (type-check?)
           (begin
             (post-load-type-check!)
             (shen-printer result (current-output-port))
             (write-string " : " (current-output-port))
             (type-printer (expression-type-check #'(#%prolog-functor fn name))
                           (current-output-port)))
           (shen-printer result (current-output-port))))]
    [((~literal datatype) name:id . _)
     (shen:eval (syntax->datum pre-eval-stx))
     (post-load-type-check!)
     (fprintf (current-output-port) "~a#type" (syntax->datum #'name))]
    [((~literal prolog?) . _)
     (shen-printer (shen:eval (syntax->datum pre-eval-stx)) (current-output-port))]
    [(~or ((~literal defmacro) name . _)
          ((~literal defprolog) name . _)
          ((~literal package) name . _))
     (let ([result (shen:eval (syntax->datum pre-eval-stx))])
       (post-load-type-check!)
       (shen-printer (syntax->datum #'name) (current-output-port)))]
    [_
     #:when (type-check?)
     (let ([type-expr (expression-type-check
                       (syntax->shen-prolog-term
                        pre-eval-stx))]
           [result (shen:eval (syntax->datum pre-eval-stx))])
       (shen-printer result (current-output-port))
       (write-string " : " (current-output-port))
       (type-printer type-expr (current-output-port)))]
    [_
     (shen-printer (shen:eval (syntax->datum pre-eval-stx)) (current-output-port))]))

(define (load filename)
  (define in (open-input-file filename))
  (parameterize ([current-readtable shen-readtable])
    (for ([stx (in-port (curry read-syntax (object-name in)) in)])
      (load-shen-form (detect-prolog-syntax (expand-shen-form stx)))
      (printf "\n")))
  (close-input-port in)
  'loaded)
