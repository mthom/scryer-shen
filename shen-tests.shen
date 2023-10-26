#lang shen

(define test-2
  4 4 X <- (fail-if (= X 2))
  1 4 5 -> fail
  4 4 X -> (* X X))

(define test-3
  X -> (let Y (+ 1 1)
         (+ Y X)))

(define test-4
  4 4 5 <- (fail-if (= 1 1))
  4 4 X -> [success: X])

(test-4 4 4 3)