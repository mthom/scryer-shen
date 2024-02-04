#lang racket

(require data/gvector
         racket
         (for-template
          racket/base
          racket/match
          racket/stxparam
          "failure.rkt"
          (only-in racket/function
                   curry))
         racket/match
         racket/syntax
         syntax/parse
         syntax/stx)

(provide function-clause-definition
         kl-defun
         macro-clause-definition
         prolog-body-pattern
         prolog-syntax-writers
         quote-pattern
         shen-binding
         shen-var-id
         shen-curry-out-export
         shen-function-out-export
         shen-define
         shen-defmacro
         shen-internal-package
         shen-package
         shen-cond-form
         shen-if-form
         shen-lambda-form
         shen-let-form
         shen-prolog-rule
         underscore-hyphen
         write-as-prolog-datum)

(define (capitalized-symbol? symbol)
  (and (symbol? symbol)
       (let ([string (symbol->string symbol)])
         (char-upper-case? (string-ref string 0)))))

(define (quote-pattern pattern)
  (syntax-parse pattern
    [((~datum cons) hd tl)
     (quasisyntax/loc pattern
       (cons #,(quote-pattern #'hd)
             #,(quote-pattern #'tl)))]
    [() #''()]
    [(~datum _) pattern]
    [id:id
     #:when (not (capitalized-symbol? (syntax->datum #'id)))
     #''id]
    [_ pattern]))

(define (underscore-hyphen atom string-port)
  (let* ([underlying-str (symbol->string (syntax->datum atom))])
    (write-string (if (equal? underlying-str "-")
                      "-"
                      (string-replace underlying-str "-" "_"))
                  string-port)))

;; write-as-prolog-datum is not redundant: runtime data need
;; special handling since they don't have the syntactic structure
;; exploited by prolog-syntax-writers at compile time.

(define (write-as-prolog-datum datum [port (open-output-string)])
  (let loop ([datum datum])
    (match datum
      [(cons hd tl)
       (write-string "[" port)
       (loop hd)
       (write-string "|" port)
       (loop tl)
       (write-string "]" port)]
      [(or '() (? void?))
       (write-string "[]" port)]
      [(? string?)
       (fprintf port "\"~s\"" datum)]
      [(== #t eq?)
       (fprintf port "true")]
      [(== #f eq?)
       (fprintf port "false")]
      [_
       (write datum port)])))

(define (prolog-syntax-writers query?)
  (define string-port (open-output-string))
  (letrec ([received-vars-vec (make-gvector)]
           [shift-args (lambda (stx [top-level-arg? #t])
                         (syntax-parse stx
                           [((~datum receive) id:shen-var-id)
                            #:when query?
                            (gvector-add! received-vars-vec #'id)
                            (datum->syntax stx '~a stx)]
                           [(~or ((~datum cons) hd tl)
                                 ((~datum #%prolog-functor) . args))
                            stx]
                           [(hd . tl)
                            #:when top-level-arg?
                            (let ([hd (shift-args #'hd #f)]
                                  [tl (stx-map (lambda (stx) (shift-args stx #f)) #'tl)]
                                  [result-binding (gensym "G")])
                              (write-string "cont:shift(bind(" string-port)
                              (write-prolog-datum hd)
                              (write-string "(" string-port)
                              (write-prolog-goals tl #f)
                              (write-string "), " string-port)
                              (write result-binding string-port)
                              (write-string ")), " string-port)

                              (datum->syntax stx result-binding stx))]
                           [form #'form]))]
           [write-prolog-datum (lambda (rule [top-level? #f])
                                 (syntax-parse rule
                                   [((~datum cons) hd tl)
                                    (let ([hd (shift-args #'hd)]
                                          [tl (shift-args #'tl)])
                                      (write-string "[" string-port)
                                      (write-prolog-datum hd)
                                      (write-string " | " string-port)
                                      (write-prolog-datum tl)
                                      (write-string "]" string-port))]
                                   [((~datum use-module) ((~datum library) lib-id:id))
                                    #:when (and top-level? query?)
                                    (write-string "use_module(library(" string-port)
                                    (underscore-hyphen #'lib-id string-port)
                                    (write-string "))" string-port)]
                                   [((~datum use-module) file-name:id)
                                    #:when (and top-level? query?)
                                    (write-string "use_module('" string-port)
                                    (write (syntax->datum #'file-name) string-port)
                                    (write-string "')" string-port)]
                                   [((~datum is!) x t)
                                    #:when top-level?
                                    (let ([x (shift-args #'x)]
                                          [t (shift-args #'t)])
                                      (write-string "unify_with_occurs_check(" string-port)
                                      (write-prolog-datum x)
                                      (write-string ", " string-port)
                                      (write-prolog-datum t)
                                      (write-string ")" string-port))]
                                   [((~datum findall) pat predicate solutions)
                                    #:when top-level?
                                    (write-string "findall(" string-port)
                                    (write-prolog-datum #'pat)
                                    (write-string ", " string-port)
                                    (write-prolog-datum #'predicate #t)
                                    (write-string ", " string-port)
                                    (write-prolog-datum #'solutions)
                                    (write-string ")" string-port)]
                                   [((~or (~datum is) (~datum bind)) x t)
                                    #:when top-level?
                                    (let ([x (shift-args #'x)]
                                          [t (shift-args #'t)])
                                      (write-string "=(" string-port)
                                      (write-prolog-datum x)
                                      (write-string ", " string-port)
                                      (write-prolog-datum t)
                                      (write-string ")" string-port))]
                                   [((~datum ~) t)
                                    #:when top-level?
                                    (let ([t (shift-args #'t)])
                                      (write-string "\\+(" string-port)
                                      (write-prolog-datum t #t)
                                      (write-string ")" string-port))]
                                   [((~datum return) t)
                                    #:when top-level?
                                    (let ([t (shift-args #'t)])
                                      (write-string "cont:shift(return_to_shen(" string-port)
                                      (write-prolog-datum t)
                                      (write-string "))" string-port))]
                                   [((~datum var?) t)
                                    #:when top-level?
                                    (let ([t (shift-args #'t)])
                                      (write-string "var(" string-port)
                                      (write-prolog-datum t))
                                    (write-string ")" string-port)]
                                   [((~datum fork) arg . args)
                                    #:when top-level?
                                    (write-string "(" string-port)
                                    (write-prolog-datum #'arg #t)
                                    (stx-map (lambda (stx)
                                               (write-string "; " string-port)
                                               (write-prolog-datum stx #t))
                                             #'args)
                                    (write-string ")" string-port)]
                                   [((~or (~datum +) (~datum -)) form)
                                    (write-prolog-datum #'form top-level?)]
                                   [((~datum when) t)
                                    #:when top-level?
                                    (let ([t (shift-args #'t)])
                                      (write-prolog-datum t)
                                      (write-string " = true" string-port))]
                                   [((~datum #%prolog-functor) id:id . args)
                                    (let ([args (stx-map shift-args #'args)])
                                      (write (syntax->datum #'id) string-port)
                                      (write-string "(" string-port)
                                      (write-prolog-goals args #f)
                                      (write-string ")" string-port))]
                                   [(hd . tl)
                                    (let ([tl (if top-level? (stx-map shift-args #'tl) #'tl)])
                                      (write-prolog-datum #'hd)
                                      (unless (stx-null? tl)
                                        (write-string "(" string-port)
                                        (write-prolog-goals tl #f)
                                        (write-string ")" string-port)))]
                                   [(~datum !)
                                    #:when top-level?
                                    (write-string "!" string-port)]
                                   [(~and atom:id (~not atom:shen-var-id))
                                    #:when (not top-level?)
                                    (underscore-hyphen #'atom string-port)]
                                   [atom
                                    (if top-level?
                                        (raise-syntax-error #f "goals must be represented as s-expressions or functors "
                                                            rule)
                                        (write-as-prolog-datum (syntax->datum #'atom) string-port))]))]
           [write-prolog-goals (lambda (arg-stx top-level?)
                                 (if (stx-pair? arg-stx)
                                     (begin
                                       (write-prolog-datum (stx-car arg-stx) top-level?)
                                       (unless (stx-null? (stx-cdr arg-stx))
                                         (write-string ", " string-port)
                                         (write-prolog-goals (stx-cdr arg-stx) top-level?)))
                                     (write-prolog-datum arg-stx top-level?)))])
    (values string-port
            write-prolog-goals
            received-vars-vec)))

(define-splicing-syntax-class shen-cond-form
  #:attributes ((condition 1)
                (true-form 1))
  (pattern (~seq (condition:expr true-form:expr) ...+)))

(define-splicing-syntax-class shen-if-form
  (pattern (~seq condition:expr true-form:expr false-form:expr)))

(define-splicing-syntax-class shen-lambda-form
  (pattern (~seq var:shen-var-id ... body-expr:expr ...+)))

(define-splicing-syntax-class shen-let-form
  #:attributes ((binding-id 1)
                (binding-expr 1)
                (body-expr 1))
  (pattern (~seq binding:shen-binding ...+ body-expr:expr ...+)
           #:with (binding-id ...)   #'(binding.id ...)
           #:with (binding-expr ...) #'(binding.expr ...)))

(define-syntax-class shen-var-id
  (pattern (~and id:id (~fail #:unless (capitalized-symbol? (syntax->datum #'id))))))

(define-syntax-class function-clause-pattern
  #:attributes (pat)
  #:datum-literals (-> <-)
  (pattern (~and (~not ->) (~not <-))
           #:with pat (quote-pattern this-syntax))
  (pattern (~or (->) (<-))
           #:with pat this-syntax))

(define-syntax-class macro-clause-pattern
  #:attributes (pat)
  #:datum-literals (->)
  (pattern (~not ->)
           #:with pat (quote-pattern this-syntax)))

(define-syntax-class prolog-head-pattern
  #:datum-literals (<--)
  (pattern (~not (~or <-- (~datum #\;)))))

(define-syntax-class prolog-body-pattern
  (pattern (~not (~datum #\;))))

(define-syntax-class shen-op-assoc
  (pattern (~or #:right #:left)))

(define-splicing-syntax-class macro-clause-definition
  #:attributes (body match-clause pat)
  #:datum-literals (->)
  (pattern (~seq pat:macro-clause-pattern -> body:expr)
           #:with match-clause #'[pat.pat body]))

(define-splicing-syntax-class shen-binding
  #:attributes (id expr)
  (pattern (~seq id:shen-var-id expr:expr)))

(define-splicing-syntax-class function-clause-definition
  #:attributes ((pats 1) arrow body guard match-clause)
  #:datum-literals (-> <- where)
  (pattern (~seq pats:function-clause-pattern ... -> body:expr
                 (~optional (~seq where guard:expr)))
           #:with arrow #'->
           #:with match-clause #'[(pats.pat ...) (~? (~@ . (#:when guard))) body])
  (pattern (~seq pats:function-clause-pattern ... <- body:expr
                 (~optional (~seq where guard:expr)))
           #:with arrow #'<-
           #:with match-clause #'[(pats.pat ...)
                                  (=> backtrack-fn)
                                  (~? (unless guard (backtrack-fn)))
                                  (syntax-parameterize ([fail (syntax-id-rules (backtrack-fn)
                                                                [(fail) (backtrack-fn)]
                                                                [fail fail])]
                                                        [fail-if (syntax-id-rules (backtrack-fn)
                                                                   [(fail-if fail-fn r)
                                                                    (let ([result r])
                                                                      (if (fail-fn result)
                                                                          (backtrack-fn)
                                                                          result))]
                                                                   [fail-if
                                                                    (lambda (e r)
                                                                      (let ([result r])
                                                                        (if (fail-fn result)
                                                                            (backtrack-fn)
                                                                            result)))])])
                                    body)]))

(define-splicing-syntax-class shen-define
  #:attributes (name (clause 1) wrapper)
  (pattern (~seq name:id clause:function-clause-definition ...+)
           #:fail-unless (apply = (map length (attribute clause.pats)))
           "each clause must contain the same number of patterns"
           #:with pats (attribute clause.pats)
           #:with (arg-id ...) (stx-map
                                (lambda (stx) (syntax-property stx 'bound #t))
                                (generate-temporaries (car (attribute clause.pats))))
           #:with wrapper #'(curry
                             (lambda (arg-id ...)
                               (match* (arg-id ...)
                                 clause.match-clause ...)))))

(define-splicing-syntax-class kl-defun
  #:attributes (name wrapper (body-expr 1))
  (pattern (~seq name:id (args:shen-var-id ...) body-expr:expr ...+)
           #:do [(stx-map
                  (lambda (stx) (syntax-property stx 'bound #t))
                  #'(args ...))]
           #:with wrapper #'(curry
                             (lambda (args ...)
                               body-expr ...))))

(define-splicing-syntax-class shen-defmacro
  #:attributes (name (pat 1) (clause-expr 1) expander)
  (pattern (~seq name:id clause:macro-clause-definition ...+)
           #:with arg-id (generate-temporary "form")
           #:with (pat ...) #'(clause.pat ...)
           #:with (clause-expr ...) #'(clause.body ...)
           #:with expander #'(lambda (k)
                               (lambda (arg-id)
                                 (match arg-id
                                   clause.match-clause
                                   ...
                                   [_ (k arg-id)])))))

(define-syntax-class shen-top-level-decl
  #:datum-literals (define defun defmacro)
  #:attributes (name expansion)
  (pattern (define define-form:shen-define)
           #:with expansion #'(define . define-form)
           #:with name (attribute define-form.name))
  (pattern (defun defun-form:kl-defun)
           #:with expansion #'(defun . defun-form)
           #:with name (attribute defun-form.name))
  (pattern (defmacro defmacro-form:shen-defmacro)
           #:with expansion #'(defmacro . defmacro-form)
           #:with name (attribute defmacro-form.name)))

(define-splicing-syntax-class shen-package
  #:attributes (export-list
                name
                (top-level-decls 1))
  (pattern (~seq name:id
                 export-list:expr
                 top-level-decls:shen-top-level-decl ...)))

(define-splicing-syntax-class shen-internal-package
  #:attributes (export-list
                external-symbols
                internal-symbols
                name
                (top-level-decls 1))
  (pattern (~seq name:id
                 export-list:expr
                 external-symbols:expr
                 internal-symbols:expr
                 top-level-decls:shen-top-level-decl ...)))

(define-syntax-class shen-curry-out-export
  #:attributes (func-id renamed-id wrapper assoc)
  (pattern [(~seq func-id:id (~optional renamed-id:id #:defaults ([renamed-id #'func-id]))
                  #:arity wrapped-arity:nat
                  (~optional (~seq #:polyadic assoc:shen-op-assoc) #:defaults ([assoc #'#f])))]
           #:fail-when (and (syntax->datum (attribute assoc))
                            (not (= (syntax->datum (attribute wrapped-arity)) 2)))
           "polyadic functions must have arity 2"
           #:with wrapper #'(curry (procedure-reduce-arity func-id wrapped-arity)))
  (pattern [(~seq func-id:id renamed-id:id)]
           #:with wrapper #'(curry func-id)
           #:with assoc #'#f)
  (pattern func-id:id
           #:with renamed-id #'func-id
           #:with wrapper #'(curry func-id)
           #:with assoc #'#f))

(define-syntax-class shen-function-out-export
  #:attributes (func-id renamed-id)
  (pattern [(~seq func-id:id renamed-id:id)])
  (pattern func-id:id
           #:with renamed-id #'func-id))

(define-splicing-syntax-class shen-prolog-rule
  #:attributes ((head-form 1)
                (body-form 1))
  #:datum-literals (<--)
  (pattern (~seq head-form:prolog-head-pattern ...
                 <--
                 body-form:prolog-body-pattern ...
                 (~datum #\;))))
