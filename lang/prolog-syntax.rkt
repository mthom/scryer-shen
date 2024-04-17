#lang racket

(require data/gvector
         (for-template racket)
         syntax/parse
         syntax/stx
         "syntax-utils.rkt")

(provide expand-shen-defprolog
         expand-shen-prolog-query
         prolog-syntax-writers
         write-as-prolog-datum)

(define (shen-atom->prolog-atom atom)
  (let* ([underlying-str (symbol->string atom)])
    (define unhyphenated-atom
      (case underlying-str
        [("-" "->" "-->")
         underlying-str]
        [else
         (string-replace underlying-str "-" "_")]))
    (if (memf (lambda (ch)
                (case ch
                  [(#\, #\; #\? #\@) #t]
                  [else #f]))
              (string->list unhyphenated-atom))
        (string-append "'" unhyphenated-atom "'")
        unhyphenated-atom)))

;; write-as-prolog-datum is not redundant: runtime data need
;; special handling since they don't have the syntactic structure
;; exploited by prolog-syntax-writers at compile time.

(define (write-as-prolog-datum datum port)
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
      [(? symbol?)
       (write-string (shen-atom->prolog-atom datum) port)]
      [(? string?)
       (fprintf port "~s" datum)]
      [(== #t eq?)
       (fprintf port "true")]
      [(== #f eq?)
       (fprintf port "false")]
      [_
       (write datum port)])))

(define (prolog-syntax-writers query? [embedded? #t])
  (define string-port (open-output-string))
  (letrec ([received-vars-vec (make-gvector)]
           [shift-args (lambda (stx [top-level-arg? #t])
                         (syntax-parse stx
                           [((~datum receive) id:shen-var-id)
                            #:when (and query? embedded?)
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
                                    (write-string (shen-atom->prolog-atom (syntax->datum #'lib-id)) string-port)
                                    (write-string "))" string-port)]
                                   [((~datum use-module) file-name:id)
                                    #:when (and top-level? query?)
                                    (write-string "use_module('" string-port)
                                    (write (syntax->datum #'file-name) string-port)
                                    (write-string "')" string-port)]
                                   [((~and slash-op (~or (~literal |\+|) (~literal |\=|) (~literal |\==|)))
                                     term)
                                    #:when top-level?
                                    ;; support ISO Prolog functors for
                                    ;; negation as failure, not
                                    ;; unifiable, not equal
                                    (write-string (symbol->string (syntax-e #'slash-op)) string-port)
                                    (write-string "(" string-port)
                                    (write-prolog-datum (shift-args #'term))
                                    (write-string ")" string-port)]
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
                                      (write-string (shen-atom->prolog-atom (syntax->datum #'id)) string-port)
                                      (unless (stx-null? #'args)
                                        (write-string "(" string-port)
                                        (write-prolog-goals args #f)
                                        (write-string ")" string-port)))]
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
                                    (write-string (shen-atom->prolog-atom (syntax->datum #'atom)) string-port)]
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

(define (expand-shen-defprolog name rules)
  (define-values (string-port write-prolog-goals received-vars-vec)
    (prolog-syntax-writers #f))

  (let ([name (shen-atom->prolog-atom (syntax->datum name))])
    (for-each (lambda (rule-stx)
                (write-string name string-port)
                (syntax-parse rule-stx
                  [(rule:shen-prolog-rule)
                   (unless (stx-null? #'(rule.head-form ...))
                     (write-string "(" string-port)
                     (write-prolog-goals #'(rule.head-form ...) #f)
                     (write-string ")" string-port))

                   (unless (stx-null? #'(rule.body-form ...))
                     (write-string " :- " string-port)
                     (write-prolog-goals #'(rule.body-form ...) #t))

                   (write-string ".\n" string-port)]))
              (syntax->list rules)))

  (get-output-string string-port))

(define (expand-shen-prolog-query query)
  (define-values (string-port write-prolog-goals received-vars-vec)
    (prolog-syntax-writers #t))

  (write-prolog-goals query #t)

  (quasisyntax/loc query
    (apply format
           #,(get-output-string string-port)
           (map
            (lambda (shen-value)
              (let ([port (open-output-string)])
                (write-as-prolog-datum shen-value port)
                (get-output-string port)))
            (list #,@(gvector->list received-vars-vec))))))
