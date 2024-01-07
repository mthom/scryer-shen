#lang racket

(compile-allow-set!-undefined #t)
(compile-enforce-module-constants #f)

(require "lang/expander.rkt"
         "lang/interposition-points.rkt"
         (only-in "lang/namespaces.rkt"
                  shen-namespace
                  kl-namespace)
         "lang/namespace-requires.rkt"
         "lang/reader.rkt"
         "lang/system-function-exports.rkt")

(define-syntax-rule (shen-mb body ...)
  (#%module-begin
   (current-namespace shen-namespace)
   body ...))

(provide (all-from-out "lang/interposition-points.rkt"
                       "lang/reader.rkt"
                       "lang/system-function-exports.rkt")
         (rename-out [app #%app]
                     [top #%top]
                     [top-interaction #%top-interaction])
         #%datum
         true
         false
         let
         /.
         define
         defmacro
         package
         (rename-out [kl-defun defun]
                     [shen-mb #%module-begin]))
