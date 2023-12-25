#lang racket

(require "namespaces.rkt")

(namespace-require "lang/interposition-points.rkt" kl-namespace)
(namespace-require "lang/system-functions.rkt" kl-namespace)
(namespace-require '(for-space function "lang/system-functions.rkt") kl-namespace)
(namespace-require '(rename "lang/expander.rkt" defun kl-defun) kl-namespace)

(namespace-require "lang/interposition-points.rkt" shen-namespace)
(namespace-require "lang/system-functions.rkt" shen-namespace)
(namespace-require '(for-space function "lang/system-functions.rkt") shen-namespace)
(namespace-require '(rename "lang/expander.rkt" defun kl-defun) shen-namespace)
(namespace-require '(only "lang/expander.rkt" define) shen-namespace)

