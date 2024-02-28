#lang racket

(compile-allow-set!-undefined #t)
(compile-enforce-module-constants #f)

(require racket/runtime-path)

(require shen/lang/interposition-points
         shen/lang/macros
         shen/lang/namespaces
         shen/lang/namespace-requires
         shen/lang/printer
         shen/lang/prolog-debug-gui
         (only-in shen/lang/reader
                  detect-prolog-syntax
                  shen-readtable)
         (only-in shen/lang/system-functions
                  [eval shen:eval])
         shen/lang/type-check
         (only-in racket/exn
                  exn->string))

(define (shen-repl)
  (define prompt-num 0)
  (open-prolog-debug-gui)
  (parameterize ([current-namespace shen-namespace]
                 [current-readtable shen-readtable])
    (let loop ()
      (with-handlers ([exn:break? (lambda (e)
                                    (write-char #\newline)
                                    (exit))]
                      [exn? (lambda (e)
                              (printf "error: ~a~n" (exn->string e))
                              (loop))])
        (printf "(~a~a) " prompt-num (if (type-check?) '+ '-))
        (set! prompt-num (add1 prompt-num))

        (shen-printer
         (shen:eval
          (syntax->datum
           (detect-prolog-syntax
            (expand-shen-form
             (read-syntax)))))
         (current-output-port))

        (printf "~n")
        (loop)))))

(shen-repl)
