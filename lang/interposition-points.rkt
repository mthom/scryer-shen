#lang racket

(require "syntax-utils.rkt"
         (only-in "system-functions.rkt" function)
         (for-syntax syntax/parse))

(provide (rename-out [app #%app]
                     [top #%top])
         #%datum
         #%top-interaction)

(define-syntax (top stx)
  (syntax-parse stx
    [(top . id:shen-var-id)
     #'(#%top . id)]
    [(top . id:id)
     (syntax/loc stx (quote id))]))

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
