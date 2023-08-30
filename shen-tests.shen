#lang shen

(define test-2
  1 2 3 -> 1
  4 4 5 <- (begin
             (printf "ehhhh\n")
             (fail-if #t))
  X X _ -> X)

(define test-3
  X -> (let Y (+ 1 1)
         (+ Y X)))