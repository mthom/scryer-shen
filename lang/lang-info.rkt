#lang racket

(define (get-info data)
  (λ (key default)
    (case key
      [(configure-runtime)
       '(#(shen/lang/configure-runtime configure #f))]
      [(color-lexer)
       (dynamic-require 'shen/tools/colorer 'shen-colorer)]
      [else
       default])))

(provide get-info)