#lang racket

(require (only-in racket/function
                  curry)
         syntax/parse
         syntax/stx
         "namespaces.rkt"
         "packages.rkt"
         "syntax-utils.rkt"
         "type-check.rkt")

(provide add-shen-macro-expander!
         remove-shen-macro-expander!
         expand-shen-form)

(struct original [form])

(struct macro-list-node [expander [prev #:mutable] [next #:mutable]]
  #:property prop:procedure (lambda (self form)
                              (((macro-list-node-expander self)
                                (if (procedure? (macro-list-node-next self))
                                    (macro-list-node-next self)
                                    original))
                               form)))

(struct macro-list [head tail] #:mutable)

(define macro-table (make-hash))

(define macro-expander-list (macro-list (void) (void)))

(define (push-macro-list! new-node)
  (unless (macro-list-node? (macro-list-head macro-expander-list))
    (set-macro-list-head! macro-expander-list new-node))

  (if (macro-list-node? (macro-list-tail macro-expander-list))
      (begin
        (set-macro-list-node-prev! new-node (macro-list-tail macro-expander-list))
        (set-macro-list-node-next! (macro-list-tail macro-expander-list) new-node)
        (set-macro-list-tail! macro-expander-list new-node))
      (set-macro-list-tail! macro-expander-list new-node)))

(push-macro-list! (macro-list-node (lambda (k) (lambda (form) (k form)))
                                   (void)
                                   (void)))

(define (add-shen-macro-expander! name new-macro-expander)
  (define new-node (macro-list-node new-macro-expander (void) (void)))

  ;; if a macro is already defined under 'name', remove it from the
  ;; macro-list.
  (when (hash-ref macro-table name (thunk #f))
    (remove-shen-macro-expander! name))

  (push-macro-list! new-node)
  (hash-set! macro-table name new-node))

(define (remove-shen-macro-expander! name)
  (let ([removed-node (hash-ref! macro-table name (thunk #f))])
    (when (macro-list-node? removed-node)
      (hash-remove! macro-table name)

      (if (macro-list-node? (macro-list-node-prev removed-node))
          (set-macro-list-node-next! (macro-list-node-prev removed-node)
                                     (macro-list-node-next removed-node))
          (set-macro-list-head! macro-expander-list
                                (macro-list-node-next removed-node)))

      (if (macro-list-node? (macro-list-node-next removed-node))
          (set-macro-list-node-prev! (macro-list-node-next removed-node)
                                     (macro-list-node-prev removed-node))
          (set-macro-list-tail! macro-expander-list
                                (macro-list-node-prev removed-node))))))

(define (shen-form-expansion-loop form)
  (if (original? form)
      (original-form form)
      (shen-form-expansion-loop ((macro-list-head macro-expander-list) form))))

(define (expand-shen-form stx)
  (syntax-parse (inner-expand-shen-form stx)
    [((~datum define) define-form:shen-define)
     #:cut
     #:fail-when (and (type-check?) (not (attribute define-form.type-sig)))
     "function needs a type signature with type checking enabled"
     this-syntax]
    [_ this-syntax]))

(define (inner-expand-shen-form stx)
  (define (expand-shen-form- stx)
    (let ([ht (make-hash)])
      (define expansion-result
        (dm-subst ht (shen-form-expansion-loop (dm-syntax->datum stx ht))))
      (if (syntax? expansion-result)
          expansion-result
          (datum->syntax stx expansion-result stx))))

  (define (expand-shen-function-clause stx)
    (syntax-parse stx
      [(clause:function-clause-definition)
       #:with expanded-body  (expand-shen-form #'clause.body)
       #:with expanded-guard (if (attribute clause.guard)
                                 (quasisyntax/loc #'clause.guard
                                   (where #,(expand-shen-form #'clause.guard)))
                                 #'())
       (syntax/loc stx
         (clause.pats ... clause.arrow expanded-body (~@ . expanded-guard)))]))

  (syntax-parse stx
    [((~datum define) define-form:shen-define)
     #:with (expanded-clauses ...) (stx-map expand-shen-function-clause #'(define-form.clause ...))
     (expand-shen-form-
      (syntax/loc stx
        (define define-form.name
          (~? (~@ |{| (~@ . define-form.type-sig) |}|))
          (~@ . expanded-clauses) ...)))]
    [((~datum defun) defun-form:kl-defun)
     #:with (expanded-body-expr ...) (stx-map inner-expand-shen-form #'(defun-form.body-expr ...))
     (expand-shen-form- (syntax/loc stx (defun defun-form.name expanded-body-expr ...)))]
    [((~datum defmacro) defmacro-form:shen-defmacro)
     #:with (expanded-clause-expr ...) (stx-map inner-expand-shen-form #'(defmacro-form.clause-expr ...))
     (expand-shen-form-
      (syntax/loc stx
        (defmacro defmacro-form.name
          (~@ defmacro-form.pat -> expanded-clause-expr) ...)))]
    [((~datum defprolog) :id :shen-prolog-rule ...+)
     (expand-shen-form- stx)]
    [((~literal cons) hd tl)
     (quasisyntax/loc stx
       (cons #,(inner-expand-shen-form #'hd)
             #,(inner-expand-shen-form #'tl)))]
    [((~datum let) let-form:shen-let-form)
     #:with (expanded-b-expr ...) (stx-map inner-expand-shen-form #'(let-form.binding-expr ...))
     #:with expanded-body-expr (inner-expand-shen-form #'let-form.body-expr)
     (expand-shen-form-
      (syntax/loc stx
        (let (~@ . [let-form.binding-id expanded-b-expr])
             ...
             expanded-body-expr)))]
    [((~datum /.) lambda-form:shen-lambda-form)
     #:with (expanded-body-expr ...) (stx-map inner-expand-shen-form #'(lambda-form.body-expr ...))
     (expand-shen-form-
      (syntax/loc stx
        (/. lambda-form.var ... expanded-body-expr ...)))]
    [((~datum package) package-form:shen-package)
     #:when (and (eq? (syntax->datum #'package-form.name) 'null)
                 (eq? (syntax->datum #'package-form.export-list) '()))
     #:with (expanded-form ...) (stx-map inner-expand-shen-form #'(package-form.top-level-decls ...))
     (syntax/loc stx
       (package null () expanded-form ...))]
    [((~datum package) package-form:shen-package)
     (let*-values ([(export-list)
                    (eval-export-list (inner-expand-shen-form #'package-form.export-list))]
                   [(top-level-forms external-symbols internal-symbols)
                    (unpackage-shen-package
                     #'package-form.name
                     export-list
                     #'(package-form.top-level-decls ...))])
       (with-syntax ([(expanded-form ...) (stx-map inner-expand-shen-form top-level-forms)]
                     [external-symbols (hash-keys external-symbols)]
                     [internal-symbols (hash-keys internal-symbols)]
                     [export-list (datum->syntax #'package-form.export-list export-list)])
         (syntax/loc stx
           (package package-form.name
                    export-list
                    external-symbols
                    internal-symbols
                    expanded-form ...))))]
    [((~datum prolog?) :prolog-body-pattern ...+)
     (expand-shen-form- stx)]
    [body:expr
     #:when (stx-pair? #'body)
     (syntax-parse (expand-shen-form- #'body)
       [(hd . tl)
        #:with expanded-car (inner-expand-shen-form #'hd)
        #:with expanded-cdr (stx-map inner-expand-shen-form #'tl)
        (if (syntax? stx)
            (syntax/loc stx (expanded-car . expanded-cdr))
            #'(expanded-car . expanded-cdr))]
       [body (inner-expand-shen-form #'body)])]
    [body:expr
     (expand-shen-form- #'body)]))

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
              [else v]))))))
