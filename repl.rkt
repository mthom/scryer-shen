#lang racket

(compile-allow-set!-undefined #t)
(compile-enforce-module-constants #f)

(require racket/runtime-path)

(require (only-in racket/exn
                  exn->string)
         shen/lang/interposition-points
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
         shen/lang/type-syntax-expanders
         syntax/parse
         syntax/stx)

(define (shen-repl)
  (define prompt-num 0)
  (open-prolog-debug-gui)

  (parameterize ([current-readtable shen-readtable])
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
        (load-shen-form (detect-prolog-syntax
                         (expand-shen-form
                          (read-syntax)))))
      (printf "~n")
      (loop))))

(shen-repl)
