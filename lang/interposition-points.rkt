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
         top)

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
