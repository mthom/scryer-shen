#lang shen

(define test-2
  4 4 X <- (fail-if (= 2) X)
  1 4 5 -> fail
  4 4 X -> (* X X))

(define test-3
  X -> (let Y (+ 1 (log 2))
         (+ Y X)))

(define test-4
  4 4 5 <- (fail-if (= 2) 2)
  4 4 X -> [success: X])

(define test-5
  X Y -> (X Y))

(define test-6
  X -> Y)

(define head
  [X | Y] -> X
  [] -> (error "head: empty list"))

(define second
  [_ Y | _] -> Y)

(define identity
  X -> X)

(identity 1)

(defmacro log-macro
  [log N] -> [+ N 10])

(define max
  X Y -> X where (> X Y)
  _ Y -> Y)

(defmacro max-macro
  [max X Y Z | W] -> [max X [max Y Z | W]])

(defmacro add1-macro
  [add1-ct X] -> (let Y [+ X 1]
                   [identity Y]))
