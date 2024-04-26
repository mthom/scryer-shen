#lang racket

(require (only-in "system-functions.rkt"
                  [eval shen:eval]
                  function)
         "load.rkt"
         "macros.rkt"
         "namespaces.rkt"
         "printer.rkt"
         "prolog.rkt"
         (only-in "reader.rkt" detect-prolog-syntax)
         (for-syntax syntax/parse
                     "syntax-utils.rkt")
         syntax/parse
         "syntax-utils.rkt"
         "type-syntax-expanders.rkt")

(provide app
         shen-toplevel-eval
         top
         top-interaction)

(define-syntax (app stx)
  (syntax-parse stx
    [(app)
     (syntax/loc stx empty)]
    [(app . (proc-var:shen-var-id . args))
     (syntax/loc stx (#%app . ((app function proc-var) . args)))]
    [(app . (proc:id . args))
     #:with fs-proc ((make-interned-syntax-introducer 'function) #'proc)
     (syntax/loc stx (#%app fs-proc . args))]
    [(app . form)
     (syntax/loc stx (#%app . form))]))

(define-syntax (top stx)
  (syntax-parse stx
    [(top . id:shen-var-id)
     (if (syntax-property #'id 'bound)
         (syntax/loc stx (#%top . id))
         (syntax/loc stx (quote id)))]
    [(top . id:id)
     (syntax/loc stx (quote id))]))

(define (shen-toplevel-eval pre-eval-stx)
  (if (type-check?)
      (syntax-parse pre-eval-stx
        [((~literal define) name:id . _)
         (let ([result (shen:eval (syntax->datum pre-eval-stx))])
           (post-load-type-check!)
           (shen-printer result (current-output-port))
           (write-string " : " (current-output-port))
           (shen-printer (expression-type-check #'(#%prolog-functor fn name))
                         (current-output-port)))]
        [((~literal datatype) name:id . _)
         (shen:eval (syntax->datum pre-eval-stx))
         (post-load-type-check!)
         (fprintf (current-output-port) "~a#type~n" (syntax->datum #'name))]
        [(~or ((~literal defmacro) name . _)
              ((~literal defprolog) name . _)
              ((~literal package) name . _)
              ((~literal prolog?) name . _))
         (let ([result (shen:eval (syntax->datum pre-eval-stx))])
           (post-load-type-check!)
           (shen-printer (syntax->datum #'name) (current-output-port)))]
        [_
         (let ([type-expr (expression-type-check
                           (syntax->shen-prolog-term
                            pre-eval-stx))]
               [result (shen:eval (syntax->datum pre-eval-stx))])
           (shen-printer result (current-output-port))
           (write-string " : " (current-output-port))
           (shen-printer type-expr (current-output-port)))])
      (let ([result (shen:eval (syntax->datum pre-eval-stx))])
        (shen-printer result (current-output-port)))))

(define-syntax (top-interaction stx)
  (syntax-parse stx
    [(top-interaction . form)
     #:with expanded-form #`(shen-toplevel-eval
                             (detect-prolog-syntax
                              (expand-shen-form #'#,#'form)))
     (syntax/loc stx
       (#%top-interaction . expanded-form))]))
