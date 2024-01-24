#lang racket

(require "namespaces.rkt"
         racket/runtime-path)

(define-runtime-module-path module-path-to-interposition-points "interposition-points.rkt")
(define-runtime-module-path module-path-to-system-function-exports "system-function-exports.rkt")
(define-runtime-module-path module-path-to-expander "expander.rkt")

(namespace-require `(rename ,module-path-to-interposition-points #%app app) kl-namespace)
(namespace-require `(rename ,module-path-to-interposition-points #%top top) kl-namespace)
(namespace-require '(only racket/base #%datum) kl-namespace)
(namespace-require module-path-to-system-function-exports kl-namespace)
(namespace-require `(for-space function ,module-path-to-system-function-exports) kl-namespace)
(namespace-require `(rename ,module-path-to-expander defun kl-defun) kl-namespace)
(namespace-require `(only ,module-path-to-expander /. false let true) kl-namespace)

(namespace-require `(rename ,module-path-to-interposition-points #%app app) shen-namespace)
(namespace-require `(rename ,module-path-to-interposition-points #%top top) shen-namespace)
(namespace-require '(only racket/base #%datum) shen-namespace)
(namespace-require module-path-to-system-function-exports shen-namespace)
(namespace-require `(for-space function ,module-path-to-system-function-exports) shen-namespace)
(namespace-require `(rename ,module-path-to-expander defun kl-defun) shen-namespace)
(namespace-require `(only ,module-path-to-expander /. define defmacro defprolog false let package prolog? true)
                   shen-namespace)
