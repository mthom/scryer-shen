#lang racket

(define (get-info data)
  (λ (key default)
    (case key
      [(configure-runtime)
       '(#(shen/lang/configure-runtime configure #f))]
      [else
       default])))

(provide get-info)