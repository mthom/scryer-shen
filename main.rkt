#lang racket

(compile-allow-set!-undefined #t)

(require "lang/expander.rkt"
         "lang/interposition-points.rkt"
         (only-in "lang/namespaces.rkt"
                  shen-namespace
                  kl-namespace)
         "lang/reader.rkt"
         "lang/system-functions.rkt"
         "lang/namespace-requires.rkt")

(define-syntax-rule (shen-mb body ...)
  (#%module-begin
   (current-namespace shen-namespace)
   body ...))

(provide (all-from-out "lang/reader.rkt"
                       "lang/interposition-points.rkt"
                       "lang/system-functions.rkt")
         true
         false
         let
         /.
         define
         (rename-out [kl-defun defun])
         require syntax
         (rename-out [shen-mb #%module-begin]))

