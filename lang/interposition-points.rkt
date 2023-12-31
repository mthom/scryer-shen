#lang racket

(require "syntax-utils.rkt"
         (only-in "system-functions.rkt"
                  function)
         (for-syntax syntax/parse))

(provide app
         top
         datum)

(define-syntax (datum stx)
  (syntax-parse stx
    [(datum . x)
     #:when (syntax-property #'x 'expanded)
     (syntax/loc stx (#%datum . x))]
    [(datum . x)
     #:with expanded-form (expand-shen-form #'x)
     (syntax/loc stx (#%datum . expanded-form))]))

(define-syntax (top stx)
  (syntax-parse stx
    [(top . id:shen-var-id)
     (if (syntax-property #'id 'bound)
         #'(#%top . id)
         (syntax/loc stx (quote id)))]
    [(top . id:id)
     (syntax/loc stx (quote id))]))

(define-syntax (app stx)
  (syntax-parse stx
    [(app)
     (syntax/loc stx empty)]
    [(app . (proc-var:shen-var-id . args))
     (syntax/loc stx (#%app . ((app function proc-var) . args)))]
    [(app . form)
     #:when (not (syntax-property #'form 'expanded))
     #:with expanded-form (expand-shen-form #'form)
     (quasisyntax/loc stx (app . expanded-form))]
    [(app . (proc:id . args))
     #:with fs-proc ((make-interned-syntax-introducer 'function) #'proc)
     (syntax/loc stx (#%app . (fs-proc . args)))]
    [(app . form)
     (syntax/loc stx (#%app . form))]))
