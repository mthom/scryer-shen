#lang racket

(require (only-in "system-functions.rkt"
                  [eval shen:eval]
                  function)
         "macros.rkt"
         (for-syntax syntax/parse
                     "syntax-utils.rkt"))

(provide app top top-interaction)

(define-syntax (app stx)
  (syntax-parse stx
    [(app)
     (syntax/loc stx empty)]
    [(app . (proc-var:shen-var-id . args))
     (syntax/loc stx (#%app . ((app function proc-var) . args)))]
    [(app . (proc:id . args))
     #:with fs-proc ((make-interned-syntax-introducer 'function) #'proc)
     (syntax/loc stx (#%app . (fs-proc . args)))]
    [(app . form)
     (syntax/loc stx (#%app . form))]))

(define-syntax (top stx)
  (syntax-parse stx
    [(top . id:shen-var-id)
     (if (syntax-property #'id 'bound)
         #'(#%top . id)
         (syntax/loc stx (quote id)))]
    [(top . id:id)
     (syntax/loc stx (quote id))]))

(define-syntax (top-interaction stx)
  (syntax-parse stx
    [(top-interaction . form)
     #:with expanded-form #`(shen:eval (syntax->datum (expand-shen-form #'#,#'form)))
     (syntax/loc stx (#%top-interaction . expanded-form))]))
