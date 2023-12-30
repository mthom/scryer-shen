#lang racket

(compile-allow-set!-undefined #t)

(require "lang/expander.rkt"
         "lang/interposition-points.rkt"
         (only-in "lang/namespaces.rkt"
                  shen-namespace
                  kl-namespace)
         "lang/namespace-requires.rkt"
         "lang/reader.rkt"
         "lang/system-functions.rkt"
         "lang/shen-cons.rkt")

(define-syntax-rule (shen-mb body ...)
  (#%module-begin
   (current-namespace shen-namespace)
   body ...))

(provide (all-from-out "lang/interposition-points.rkt"
                       "lang/reader.rkt"
                       "lang/system-functions.rkt")
         true
         false
         let
         /.
         define
         require syntax
         (rename-out [kl-defun defun]
                     [shen-mb #%module-begin]))
