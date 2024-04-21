#lang racket

(require (except-in racket
                    load
                    foldr
                    map
                    eval
                    set
                    vector)
         (only-in "expander.rkt"
                  shen-curry-out
                  shen-function-out)
         "failure.rkt"
         (only-in "load.rkt"
                  load)
         "system-functions.rkt"
         "systemf.rkt")

;; system functions manifest
(provide (shen-curry-out [+ #:arity 2 #:polyadic #:right]
                         [* #:arity 2 #:polyadic #:right]
                         [- #:arity 2]
                         [/ #:arity 2]
                         [> #:arity 2]
                         [< #:arity 2]
                         [equal? = #:arity 2]
                         [equal? == #:arity 2]
                         [foldr #:arity 3]
                         [>= #:arity 2]
                         [<= #:arity 2]
                         [make-vector absvector #:arity 1]
                         [<-vector #:arity 2]
                         [vector-> #:arity 3]
                         [cons adjoin #:arity 2]
                         [append #:arity 2 #:polyadic #:right]
                         [map #:arity 2]
                         [cn #:arity 2]
                         [concat #:arity 2]
                         [difference #:arity 2]
                         [element? #:arity 2])
         (rename-out [begin do]
                     [shen-and and]
                     [shen-or or])
         (shen-function-out [car hd]
                            cd
                            [cdr tl]
                            [vector? absvector?]
                            arity
                            bound?
                            cons
                            cons?
                            empty?
                            error-to-string
                            external
                            eval
                            eval-kl
                            function
                            internal
                            limit
                            load
                            output
                            symbol?
                            systemf
                            thaw
                            undefmacro
                            value
                            vector)
         (all-from-out "failure.rkt")
         destroy
         error
         freeze
         set
         tc)
