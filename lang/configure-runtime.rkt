#lang racket

(provide configure)

(require (only-in "reader.rkt" shen-readtable)
         "printer.rkt")

(define (configure data)
  (global-port-print-handler shen-printer)
  (current-read-interaction read-one-line))

(define (read-one-line origin port)
  (parameterize ([current-readtable shen-readtable])
    (read-syntax origin port)))
