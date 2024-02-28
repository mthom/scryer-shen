#lang racket

(compile-allow-set!-undefined #t)
(compile-enforce-module-constants #f)

(require shen/lang/expander
         shen/lang/interposition-points
         shen/lang/namespaces
         shen/lang/reader
         shen/lang/system-function-exports)

(namespace-require '(rename shen/lang/interposition-points #%app app) kl-namespace)
(namespace-require '(rename shen/lang/interposition-points #%top top) kl-namespace)
(namespace-require '(only racket/base #%datum) kl-namespace)
(namespace-require 'shen/lang/system-function-exports kl-namespace)
(namespace-require '(for-space function shen/lang/system-function-exports) kl-namespace)
(namespace-require '(rename shen/lang/expander defun kl-defun) kl-namespace)
(namespace-require '(only shen/lang/expander /. cond false if let true) kl-namespace)

(namespace-require '(rename shen/lang/interposition-points #%app app) shen-namespace)
(namespace-require '(rename shen/lang/interposition-points #%top top) shen-namespace)
(namespace-require '(only racket/base #%datum) shen-namespace)
(namespace-require 'shen/lang/system-function-exports shen-namespace)
(namespace-require '(for-space function shen/lang/system-function-exports) shen-namespace)
(namespace-require '(rename shen/lang/expander defun kl-defun) shen-namespace)
(namespace-require '(only shen/lang/expander /. cond define defmacro defprolog false let package prolog? true)
                   shen-namespace)

(define-syntax-rule (shen-mb body ...)
  (#%module-begin
   (current-namespace shen-namespace)
   body ...))

(provide (all-from-out shen/lang/interposition-points
                       shen/lang/reader
                       shen/lang/system-function-exports)
         (rename-out [app #%app]
                     [top #%top]
                     [top-interaction #%top-interaction])
         #%datum
         true
         false
         cond
         if
         let
         <>
         @p
         @s
         @v
         /.
         define
         defmacro
         defprolog
         package
         prolog?
         (rename-out [kl-defun defun]
                     [shen-mb #%module-begin]))
