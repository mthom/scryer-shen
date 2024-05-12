#lang racket

(require "prolog-debug-gui.rkt"
         (only-in "prolog-syntax.rkt"
                  write-as-prolog-datum)
         (only-in racket/exn
                  exn->string)
         racket/sandbox
         (only-in "reader.rkt" shen-readtable)
         "scryer-prolog-interface.rkt"
         (prefix-in shen: "system-functions.rkt"))

(provide add-prolog-predicate!
         run-prolog-query!)

(define (peek-for-prolog-warning)
  (parameterize ([current-readtable shen-readtable])
    (when (eq? (peek-char scryer-prolog-in) #\%)
      (read-line scryer-prolog-in)
      (peek-for-prolog-warning))))

(define (add-prolog-predicate! iso-prolog-code)
  (fprintf scryer-prolog-log-out "?- ")
  (fprintf scryer-prolog-out "[user].~n~a~nend_of_file.~n" iso-prolog-code))

(define (run-prolog-query! iso-prolog-query)
  (fprintf scryer-prolog-log-out "?- ")
  (fprintf scryer-prolog-out "shen_prolog_eval((~a)).~n" iso-prolog-query)

  (with-handlers ([exn:fail? (lambda (e)
                               (printf "prolog error on query ~a: ~a~n" iso-prolog-query (exn->string e))
                               (read-char scryer-prolog-in) ;; read trailing newline
                               #f)])
    (let loop ()
      (peek-for-prolog-warning)
      (match (parameterize ([current-readtable shen-readtable])
               (shen:eval (read scryer-prolog-in)))
        [(list fn-call continue?)
         (read-char scryer-prolog-in) ;; read trailing newline
         (if continue?
             (begin
               (write-as-prolog-datum (shen:eval fn-call) scryer-prolog-out)
               (fprintf scryer-prolog-out ".~n")
               (loop))
             fn-call)]
        [_  #f]))))
