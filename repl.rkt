#lang racket

(compile-allow-set!-undefined #t)
(compile-enforce-module-constants #f)

(require racket/runtime-path)

(require shen/lang/interposition-points
         shen/lang/load
         shen/lang/macros
         shen/lang/namespaces
         shen/lang/namespace-requires
         shen/lang/printer
         shen/lang/prolog-debug-gui
         (only-in shen/lang/reader
                  detect-prolog-syntax
                  shen-readtable)
         (only-in shen/lang/syntax-utils
                  syntax->shen-prolog-term)
         (only-in shen/lang/system-functions
                  [eval shen:eval])
         shen/lang/type-check
         (only-in racket/exn
                  exn->string)
         syntax/parse
         syntax/stx)

(define (shen-repl)
  (define prompt-num 0)
  (open-prolog-debug-gui)

  (parameterize ([current-namespace shen-namespace]
                 [current-readtable shen-readtable])
    (let loop ()
      (with-handlers ([shen-type-check-exn? (lambda (e)
                                              (printf "type error\n"))]
                      [exn:break? (lambda (e)
                                    (write-char #\newline)
                                    (exit))]
                      [exn? (lambda (e)
                              (printf "error: ~a~n" (exn->string e)))])
        (printf "(~a~a) " prompt-num (if (type-check?) '+ '-))
        (set! prompt-num (add1 prompt-num))

        (let ([pre-eval-syntax (detect-prolog-syntax
                                (expand-shen-form
                                 (read-syntax)))])
          (if (type-check?)
              (syntax-parse pre-eval-syntax
                [((~literal define) name . _)
                 (let ([result (shen:eval (syntax->datum pre-eval-syntax))])
                   (post-load-type-check!)
                   (shen-printer (syntax->datum #'name) (current-output-port))
                   (write-string " : " (current-output-port))
                   (shen-printer (expression-type-check #'(#%prolog-functor fn name))
                                 (current-output-port)))]
                [(~or ((~literal datatype) name . _))
                 (shen:eval (syntax->datum pre-eval-syntax))
                 (post-load-type-check!)
                 (fprintf (current-output-port) "~a#type~n" (syntax->datum #'name))]
                [(~or ((~literal datatype) name . _)
                      ((~literal defmacro) name . _)
                      ((~literal defprolog) name . _)
                      ((~literal package) name . _)
                      ((~literal prolog?) name . _))
                 (let ([result (shen:eval (syntax->datum pre-eval-syntax))])
                   (post-load-type-check!)
                   (shen-printer (syntax->datum #'name) (current-output-port)))]
                [_
                 (let ([type-expr (expression-type-check
                                   (syntax->shen-prolog-term
                                    pre-eval-syntax))]
                       [result (shen:eval (syntax->datum pre-eval-syntax))])
                   (shen-printer result (current-output-port))
                   (write-string " : " (current-output-port))
                   (shen-printer type-expr (current-output-port)))])
              (let ([result (shen:eval (syntax->datum pre-eval-syntax))])
                (shen-printer result (current-output-port))))))
      (printf "~n")
      (loop))))

(shen-repl)
