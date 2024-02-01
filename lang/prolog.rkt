#lang racket

(require "namespaces.rkt"
         "prolog-debug-gui.rkt"
         (only-in racket/exn
                  exn->string)
         (only-in "reader.rkt" shen-readtable)
         "scryer-prolog-interface.rkt"
         (only-in "syntax-utils.rkt" write-as-prolog-datum)
         (prefix-in shen: "system-functions.rkt")
         (for-syntax
          data/gvector
          racket
          "syntax-utils.rkt"
          syntax/parse
          syntax/stx))

(provide add-prolog-predicate!
         run-prolog-query!
         (for-syntax expand-shen-defprolog
                     expand-shen-prolog-query))

(define (peek-for-prolog-warning)
  (when (eq? (peek-char scryer-prolog-in) #\%)
    (read-line scryer-prolog-in)
    (peek-for-prolog-warning)))

(define (add-prolog-predicate! iso-prolog-code)
  (fprintf scryer-prolog-log-out "?- ")
  (fprintf scryer-prolog-out "[user].~n~a~nend_of_file.~n" iso-prolog-code))

(define (run-prolog-query! iso-prolog-query)
  (fprintf scryer-prolog-log-out "?- ")
  (fprintf scryer-prolog-out "shen_prolog_eval((~a)).~n" iso-prolog-query)

  (begin0
      (with-handlers ([(const #t) (lambda (e)
                                    (printf "prolog error: ~a~n" (exn->string e))
                                    (read-char scryer-prolog-in) ;; read trailing newline
                                    #f)])
        (let loop ()
          (peek-for-prolog-warning)
          (match (parameterize ([current-readtable shen-readtable])
                   (shen:eval (read scryer-prolog-in)))
            [(cons fn-call (cons continue? empty))
             (read-char scryer-prolog-in) ;; read trailing newline
             (if continue?
                 (begin
                   (write-as-prolog-datum (shen:eval fn-call) scryer-prolog-out)
                   (fprintf scryer-prolog-out ".~n")
                   (loop))
                 fn-call)]
            [_ #f])))
    ;; read in solutions line, which scryer-shen does not use
    ;;(read-line scryer-prolog-in)
    ))

(begin-for-syntax
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
      (apply format
             #,(get-output-string string-port)
             (map
              (lambda (shen-value)
                (let ([port (open-output-string)])
                  (write-as-prolog-datum shen-value port)
                  (get-output-string port)))
              (list #,@(gvector->list received-vars-vec)))))))
