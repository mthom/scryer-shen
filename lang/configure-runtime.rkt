#lang racket

(provide configure)

(require "reader.rkt"
         (only-in racket [read-syntax r:read-syntax])
         syntax/strip-context)

(define (configure data)
  (current-read-interaction read-one-line))

(define (read-one-line origin port)
  (parameterize ([current-readtable shen-readtable])
    (strip-context (r:read-syntax origin port))))