#lang racket

(require "namespaces.rkt")

(namespace-require '(rename "lang/interposition-points.rkt" #%app app) kl-namespace)
(namespace-require '(rename "lang/interposition-points.rkt" #%top top) kl-namespace)
(namespace-require '(only racket/base #%datum) kl-namespace)
(namespace-require "lang/system-function-exports.rkt" kl-namespace)
(namespace-require '(for-space function "lang/system-function-exports.rkt") kl-namespace)
(namespace-require '(rename "lang/expander.rkt" defun kl-defun) kl-namespace)

(namespace-require '(rename "lang/interposition-points.rkt" #%app app) shen-namespace)
(namespace-require '(rename "lang/interposition-points.rkt" #%top top) shen-namespace)
(namespace-require '(only racket/base #%datum) shen-namespace)
(namespace-require "lang/system-function-exports.rkt" shen-namespace)
(namespace-require '(for-space function "lang/system-function-exports.rkt") shen-namespace)
(namespace-require '(rename "lang/expander.rkt" defun kl-defun) shen-namespace)
(namespace-require '(only "lang/expander.rkt" define defmacro) shen-namespace)

