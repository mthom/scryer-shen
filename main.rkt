#lang racket

(require "reader.rkt" "expander.rkt"
         (only-in racket [define r:define]
                         [read-syntax r:read-syntax])
         syntax/strip-context)

(provide (all-from-out "reader.rkt" "expander.rkt"))

(r:define (read-syntax path port)
  (r:define shen-datums
    (parameterize ([current-readtable shen-readtable])
      (for/list ([term (in-port (curry r:read-syntax path) port)])
        #:break (eof-object? term)
        term)))
  (strip-context
   #`(module shen-mod shen
       #,@shen-datums)))

(module+ reader
  (provide read-syntax))

(r:define (read-one-line origin port)
  (parameterize ([current-readtable shen-readtable])
    (strip-context (r:read-syntax origin port))))

(define-syntax-rule (shen-mb . exprs)
  (#%module-begin
    (current-read-interaction read-one-line)
    . exprs))

;; arithmetic operators
(provide + - * /)

;; list and tuples
(r:define hd car)
(r:define tl cdr)
(r:define (@p . args) (cons '@p args))

(provide hd tl @p cons (rename-out [shen-mb #%module-begin]))

;; debug
;;(provide require syntax)