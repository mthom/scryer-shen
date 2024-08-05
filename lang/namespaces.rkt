#lang racket

(provide kl-namespace
         shen-namespace
         shen-prolog-namespace)

(define-namespace-anchor kl-namespace-anchor)

(define kl-namespace (namespace-anchor->empty-namespace kl-namespace-anchor))
(define shen-namespace (namespace-anchor->empty-namespace kl-namespace-anchor))
(define shen-prolog-namespace (namespace-anchor->empty-namespace kl-namespace-anchor))
