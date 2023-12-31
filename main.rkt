#lang racket

(compile-allow-set!-undefined #t)

(require "lang/expander.rkt"
         (for-syntax (only-in "lang/expander.rkt"
                              let
                              /.
                              true
                              false))
         "lang/interposition-points.rkt"
         (for-syntax "lang/interposition-points.rkt")
         (only-in "lang/namespaces.rkt"
                  shen-namespace
                  kl-namespace)
         "lang/namespace-requires.rkt"
         "lang/reader.rkt"
         "lang/system-function-exports.rkt"
         (for-syntax "lang/system-function-exports.rkt")
         "lang/shen-cons.rkt")

(define-syntax-rule (shen-mb body ...)
  (#%module-begin
   (current-namespace shen-namespace)
   body ...))

(provide (all-from-out "lang/interposition-points.rkt"
                       "lang/reader.rkt"
                       "lang/system-function-exports.rkt")
         (for-syntax (all-from-out "lang/system-function-exports.rkt"))
         (rename-out [app #%app]
                     [top #%top]
                     [datum #%datum])
         #%top-interaction
         (for-syntax (rename-out [app #%app]
                                 [top #%top]
                                 [datum #%datum])
                     let
                     /.
                     true
                     false)
         true
         false
         let
         /.
         define
         defmacro
         (rename-out [kl-defun defun]
                     [shen-mb #%module-begin]))
