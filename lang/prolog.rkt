#lang racket

(require "namespaces.rkt"
         "prolog-debug-gui.rkt"
         (only-in "prolog-syntax.rkt"
                  write-as-prolog-datum)
         (only-in racket/exn
                  exn->string)
         "racket-iso-prolog-term-interface.rkt"
         "scryer-prolog-interface.rkt")

(provide add-prolog-predicate!
         run-prolog-query!)

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

  (with-handlers ([exn:fail? (lambda (e)
                               (printf "prolog error on query ~a: ~a~n"
                                       iso-prolog-query (exn->string e))
                               (read-char scryer-prolog-in) ;; read trailing newline
                               #f)])
    (let loop ()
      (peek-for-prolog-warning)
      (match (read-iso-prolog-term scryer-prolog-in)
        [(list (list 'type-functor type))
         type]
        [(list 'type-check-error _)
         #f]
        [(or (list fn-call continue?)
             (list 'cons fn-call (list 'cons continue? '())))
         (define result (eval fn-call shen-namespace))
         (if (eq? continue? 'true)
             (begin
               (write-as-prolog-datum result scryer-prolog-out)
               (fprintf scryer-prolog-out ".~n")
               (loop))
             result)]
        [_ #f]))))
