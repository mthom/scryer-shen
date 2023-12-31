#lang racket

(require (for-syntax racket
                     syntax/stx))

(begin-for-syntax
  (define macro-expander identity)

  (struct original [form] #:transparent)

  (define (add-shen-macro-expander! new-macro)
    (set! macro-expander (compose macro-expander new-macro)))

  (define (shen-form-expansion-loop form)
    (let loop ([form form])
      (if (original? form)
          (original-form form)
          (loop ((macro-expander original) form)))))

  (define (expand-shen-form stx)
    (let ([ht (make-hash)])
      (define expansion-result
        (dm-subst ht (shen-form-expansion-loop (dm-syntax->datum stx ht))))
      (syntax-property
       (if (syntax? expansion-result)
           expansion-result
           (datum->syntax stx expansion-result stx))
       'expanded
       #t)))

  ;; from compatibility-lib/define-macro, used to deconstruct
  ;; and reconstruct syntax objects.

  (define (dm-syntax->datum stx ht)
    ;; Easiest to handle cycles by letting `syntax-object->datum'
    ;;  do all the work.
    (let ([v (syntax->datum stx)])
      (let loop ([stx stx][v v])
        (let ([already (hash-ref ht v (lambda () #f))])
          (if already
              (hash-set! ht v #t) ;; not stx => don't subst later
              (hash-set! ht v stx))
          (cond
           [(stx-pair? stx)
            (loop (stx-car stx) (car v))
            (loop (stx-cdr stx) (cdr v))]
           [(stx-null? stx) null]
           [(vector? (syntax-e stx))
            (for-each
             loop
             (vector->list
              (syntax-e stx))
             (vector->list v))]
           [(box? (syntax-e stx))
            (loop (unbox (syntax-e stx))
                  (unbox v))]
           [else (void)])))
      v))

  (define (dm-subst ht v)
    (define cycle-ht (make-hash))
    (let loop ([v v])
      (if (hash-ref cycle-ht v (lambda () #f))
          v
          (begin
            (hash-set! cycle-ht v #t)
            (let ([m (hash-ref ht v (lambda () #f))])
              (cond
               [(syntax? m) m] ;; subst back!
               [(pair? v) (cons (loop (car v))
                                (loop (cdr v)))]
               [(vector? v) (list->vector
                             (map
                              loop
                              (vector->list v)))]
               [(box? v) (box (loop (unbox v)))]
               [else v])))))))

(provide (for-syntax add-shen-macro-expander!
                     expand-shen-form))
