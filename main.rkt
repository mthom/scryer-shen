#lang racket

(compile-allow-set!-undefined #t)
(compile-enforce-module-constants #f)

(require "lang/expander.rkt"
         "lang/interposition-points.rkt"
         "lang/namespaces.rkt"
         "lang/reader.rkt"
         "lang/system-function-exports.rkt")

(namespace-require '(rename "lang/interposition-points.rkt" #%app app) kl-namespace)
(namespace-require '(rename "lang/interposition-points.rkt" #%top top) kl-namespace)
(namespace-require '(only racket/base #%datum) kl-namespace)
(namespace-require "lang/system-function-exports.rkt" kl-namespace)
(namespace-require '(for-space function "lang/system-function-exports.rkt") kl-namespace)
(namespace-require '(rename "lang/expander.rkt" defun kl-defun) kl-namespace)
(namespace-require '(only "lang/expander.rkt" /. cond false if let true) kl-namespace)

(namespace-require '(rename "lang/interposition-points.rkt" #%app app) shen-namespace)
(namespace-require '(rename "lang/interposition-points.rkt" #%top top) shen-namespace)
(namespace-require '(only racket/base #%datum) shen-namespace)
(namespace-require "lang/system-function-exports.rkt" shen-namespace)
(namespace-require '(for-space function "lang/system-function-exports.rkt") shen-namespace)
(namespace-require '(rename "lang/expander.rkt" defun kl-defun) shen-namespace)
(namespace-require '(only "lang/expander.rkt" /. cond define defmacro defprolog false let package prolog? true)
                   shen-namespace)

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
