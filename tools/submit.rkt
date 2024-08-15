#lang racket

(provide repl-submit?)

(require syntax/readerr)

(define (repl-submit? ip has-white-space?)
  (define paren-stack-depth 0)
  (let loop ([in-comment? #f])
    (define c (read-char ip))
    (cond
      [(eof-object? c) (zero? paren-stack-depth)]
      [(eqv? c #\*) (if (and in-comment? (eqv? (peek-char ip) #\\))
                        (loop #f)
                        (loop in-comment?))]
      [in-comment? (loop #t)]
      [(or (eqv? c #\() (eqv? c #\[))
       (if in-comment?
           (loop #t)
           (begin
             (set! paren-stack-depth (add1 paren-stack-depth))
             (loop #f)))]
      [(or (eqv? c #\)) (eqv? c #\]))
       (if in-comment?
           (loop #t)
           (begin
             (set! paren-stack-depth (sub1 paren-stack-depth))
             (if (< paren-stack-depth 0)
                 (let-values ([(line col pos) (port-next-location ip)])
                   (raise-read-error "expected an opening '('" #f line col pos 1))
                 (loop #f))))]
      [(eqv? c #\|) (loop #f)]
      [(eqv? c #\newline) (if (= paren-stack-depth 0)
                              (loop #f)
                              (loop #f))]
      [(char-whitespace? c) (loop #f)]
      [else (loop #f)])))

(module+ test
  (require rackunit)

  (define (test s)
    (repl-submit? (open-input-string s) #f))
  ;; evaluate to true to force evaluation in the REPL.
  (check-true (test "symbol\n"))
  (check-true (test "symbol"))
  (check-false (test "[1 ")))
