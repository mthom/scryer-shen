#lang racket

(define-namespace-anchor kl-namespace-anchor)

(define kl-namespace (namespace-anchor->empty-namespace kl-namespace-anchor))
(define shen-namespace (namespace-anchor->empty-namespace kl-namespace-anchor))

(provide kl-namespace
         shen-namespace)
