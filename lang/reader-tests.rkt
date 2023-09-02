#lang racket

(require rackunit "reader.rkt")

(check-equal? (read (open-input-string "[1 2 3 | 4]")) '(1 2 3 . 4))
(check-equal? (read (open-input-string "[[1]]")) '((1)))
(check-equal? (read (open-input-string "[2 [1]]")) '(2 (1)))
(check-equal? (read (open-input-string "[[1 2] | 3]")) '((1 2) . 3))
(check-equal? (read (open-input-string "[[1 2] | [3 4]]")) '((1 2) 3 4))
(check-equal? (read (open-input-string "[1 2 | [3 4]]")) '(1 2 3 4))
(check-equal? (read (open-input-string "[]")) '())
(check-equal? (read (open-input-string "[[]]")) '(()))
(check-exn exn:fail:read? (thunk (read (open-input-string "[1 2 3 | 4 5]"))))
(check-exn exn:fail:read? (thunk (read (open-input-string "[1 2 3 |]"))))