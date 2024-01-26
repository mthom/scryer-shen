#lang racket

(compile-allow-set!-undefined #t)
(compile-enforce-module-constants #f)

(require racket/runtime-path)

(require "lang/interposition-points.rkt"
         "lang/macros.rkt"
         "lang/namespaces.rkt"
         "lang/namespace-requires.rkt"
         "lang/printer.rkt"
         "lang/prolog-debug-gui.rkt"
         (only-in "lang/reader.rkt"
                  shen-readtable)
         (only-in "lang/system-functions.rkt"
                  [eval shen:eval])
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
        (printf "(~a-) " prompt-num)

        (set! prompt-num (add1 prompt-num))

        (shen-printer
         (shen:eval
          (syntax->datum
           (expand-shen-form (read-syntax))))
         (current-output-port))

        (printf "~n")
        (loop)))))

(shen-repl)
