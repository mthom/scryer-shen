#lang shen

(define test-2
  4 4 X <- (fail) where (= X 3)
  4 4 5 -> succeed
  4 4 X -> X)

(define test-3
  X -> (let Y (+ 1 1)
         (+ Y X)))

(define test-4
  4 4 5 <- (fail-if (= 1 1))
  4 4 5 -> succeed)

(test-2 4 4 3)