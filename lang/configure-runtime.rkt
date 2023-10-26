#lang racket

(provide configure)

(require "reader.rkt" "printer.rkt"
         (only-in racket [read-syntax r:read-syntax])
         syntax/strip-context)

(define (configure data)
  (global-port-print-handler shen-printer)
  (current-read-interaction read-one-line))

(define (read-one-line origin port)
  (parameterize ([current-readtable shen-readtable])
    (strip-context (r:read-syntax origin port))))