#lang racket

(require data/gvector
         "prolog-syntax.rkt"
         (for-template (only-in "prolog-syntax.rkt"
                                write-as-prolog-datum)
                       racket)
         syntax/parse
         syntax/stx
         "syntax-utils.rkt")

(provide expand-shen-defprolog
         expand-shen-prolog-query)

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
