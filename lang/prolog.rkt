#lang racket

(require "namespaces.rkt"
         "printer.rkt"
         (only-in "reader.rkt" shen-readtable)
         "scryer-prolog-interface.rkt"
         (prefix-in shen: "system-functions.rkt")
         (for-syntax
          data/gvector
          "printer.rkt"
          racket
          "syntax-utils.rkt"
          syntax/parse
          syntax/stx))

(provide add-prolog-predicate!
         run-prolog-query!
         (for-syntax expand-shen-defprolog
                     expand-shen-prolog-query))

(define (add-prolog-predicate! iso-prolog-code)
  (fprintf scryer-prolog-out "[user].~n~a~nend_of_file.~n" iso-prolog-code))

(define (run-prolog-query! iso-prolog-query)
  (fprintf scryer-prolog-out "shen_prolog_eval((~a)).~n" iso-prolog-query)
  (begin0
      (let loop ()
        (match (parameterize ([current-readtable shen-readtable])
                 (shen:eval (read scryer-prolog-in)))
          [(cons fn-call (cons continue? empty))
           (read-char scryer-prolog-in) ;; read trailing newline
           (define result (shen:eval fn-call))
           (if continue?
               (begin
                 (shen-printer result scryer-prolog-out)
                 (fprintf scryer-prolog-out ".~n")
                 (loop))
               result)]
          [_ #f]))
    ;; read in solutions line, which scryer-shen does not use
    (read-line scryer-prolog-in)))

(begin-for-syntax
  (define (underscore-hyphen atom string-port)
    (let* ([underlying-str (symbol->string (syntax->datum atom))])
      (write-string (if (equal? underlying-str "-")
                        "-"
                        (string-replace underlying-str "-" "_"))
                    string-port)))

  (define (prolog-syntax-writers query?)
    (define string-port (open-output-string))
    (letrec ([received-vars-vec (make-gvector)]
             [shift-args (lambda (stx [top-level-arg? #t])
                           (syntax-parse stx
                             [((~datum receive) id:shen-var-id)
                              #:when query?
                              (gvector-add! received-vars-vec #'id)
                              (datum->syntax stx '~a stx)]
                             [((~datum cons) hd tl)
                              (let ([hd (shift-args #'hd top-level-arg?)]
                                    [tl (shift-args #'tl top-level-arg?)])
                                (quasisyntax/loc stx (cons #,hd #,tl)))]
                             [(hd . tl)
                              #:when top-level-arg?
                              (let ([hd (shift-args #'hd #f)]
                                    [tl (stx-map (lambda (stx) (shift-args stx #f)) #'tl)]
                                    [result-binding (gensym "G")])
                                (write-string "shift(bind(" string-port)
                                (loop hd)
                                (write-string "(" string-port)
                                (write-prolog-goals tl #f)
                                (write-string "), " string-port)
                                (write result-binding string-port)
                                (write-string ")), " string-port)

                                (datum->syntax stx result-binding stx))]
                             [form #'form]))]
             [loop (lambda (rule [top-level? #f])
                     (syntax-parse rule
                       [((~datum cons) hd tl)
                        (let ([hd (shift-args #'hd)]
                              [tl (shift-args #'tl)])
                          (write-string "[" string-port)
                          (loop hd)
                          (write-string " | " string-port)
                          (loop tl)
                          (write-string "]" string-port))]
                       [((~datum is!) x t)
                        #:when top-level?
                        (let ([x (shift-args #'x)]
                              [t (shift-args #'t)])
                          (write-string "unify_with_occurs_check(" string-port)
                          (loop x)
                          (write-string ", " string-port)
                          (loop t)
                          (write-string ")" string-port))]
                       [((~datum findall) pat predicate solutions)
                        #:when top-level?
                        (write-string "findall(" string-port)
                        (loop #'pat)
                        (write-string ", " string-port)
                        (loop #'predicate #t)
                        (write-string ", " string-port)
                        (loop #'solutions)
                        (write-string ")" string-port)]
                       [((~or (~datum is) (~datum bind)) x t)
                        #:when top-level?
                        (let ([x (shift-args #'x)]
                              [t (shift-args #'t)])
                          (write-string "=(" string-port)
                          (loop x)
                          (write-string ", " string-port)
                          (loop t)
                          (write-string ")" string-port))]
                       [((~datum ~) t)
                        #:when top-level?
                        (let ([t (shift-args #'t)])
                          (write-string "\\+(" string-port)
                          (loop t #t)
                          (write-string ")" string-port))]
                       [((~datum return) t)
                        #:when top-level?
                        ;; Shen Prolog doesn't have non-list functors
                        ;; so there's no way to spoof the return_to_shen/1
                        ;; convention.
                        (let ([t (shift-args #'t)])
                          (write-string "shift(return_to_shen(" string-port)
                          (loop t)
                          (write-string "))" string-port))]
                       [((~datum var?) t)
                        #:when top-level?
                        (let ([t (shift-args #'t)])
                          (write-string "var(" string-port)
                          (loop t))
                        (write-string ")" string-port)]
                       [((~datum fork) arg . args)
                        #:when top-level?
                        (write-string "(" string-port)
                        (loop #'arg #t)
                        (stx-map (lambda (stx)
                                   (write-string "; " string-port)
                                   (loop stx #t))
                                 #'args)
                        (write-string ")" string-port)]
                       [((~or (~datum +) (~datum -)) form)
                        (loop #'form top-level?)]
                       [((~datum when) t)
                        #:when top-level?
                        (let ([t (shift-args #'t)])
                          (loop t)
                          (write-string " = true" string-port))]
                       [(hd . tl)
                        (let ([tl (if top-level? (stx-map shift-args #'tl) #'tl)])
                          (loop #'hd)
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
                            (raise-syntax-error #f "goals must be represented as s-expressions")
                            (shen-printer (syntax->datum #'atom) string-port))]))]
             [write-prolog-goals (lambda (arg-stx top-level?)
                                   (if (stx-pair? arg-stx)
                                       (begin
                                         (loop (stx-car arg-stx) top-level?)
                                         (unless (stx-null? (stx-cdr arg-stx))
                                           (write-string ", " string-port)
                                           (write-prolog-goals (stx-cdr arg-stx) top-level?)))
                                       (loop arg-stx top-level?)))])
      (values string-port
              write-prolog-goals
              received-vars-vec)))

  (define (expand-shen-defprolog name rules)
    (define-values (string-port write-prolog-goals received-vars-vec)
      (prolog-syntax-writers #f))

    (for-each (lambda (rule-stx)
                (underscore-hyphen name string-port)
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
              (syntax->list rules))

    (get-output-string string-port))

  (define (expand-shen-prolog-query query)
    (define-values (string-port write-prolog-goals received-vars-vec)
      (prolog-syntax-writers #t))

    (write-prolog-goals query #t)

    (quasisyntax/loc query
      (format #,(get-output-string string-port)
              #,@(gvector->list received-vars-vec)))))
