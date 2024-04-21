#lang racket

(require "pairs.rkt"
         racket/base
         racket/generator
         racket/match)

(provide shen-printer)

(define (print-cons-contents args port)
  (for ([arg (in-generator
              (let loop ([pair args])
                (cond [(cons? pair)
                       (yield (car pair))
                       (loop (cdr pair))]
                      [else
                       (unless (empty? pair)
                         (write-string " |" port)
                         (yield pair))])))]
        [space (sequence-append (in-value "") (in-cycle '(#\space)))])
    (fprintf port "~a" space)
    (shen-printer arg port)))

(define/contract (shen-printer datum port)
  (any/c output-port? . -> . any)
  (match datum
    [(cons '#%type-functor (? cons? type))
     (write-string "(" port)
     (print-cons-contents type port)
     (write-string ")" port)]
    [(cons '#%type-functor type)
     (shen-printer type port)]
    [(? empty?)
     (write-string "[]" port)]
    [(? cons?)
     (write-char #\[ port)
     ;; don't print a space before the first element but do print one
     ;; before every subsequent element
     (print-cons-contents datum port)
     (write-char #\] port)]
    [(? symbol?)
     (write-string (symbol->string datum) port)]
    [(? string?)
     (write-char #\" port)
     (write-string datum port)
     (write-char #\" port)]
    [(? boolean?)
     (write-string (if datum "true" "false") port)]
    [(? void?)
     (write-string "[]" port)]
    [(? shen-tuple?)
     (write-string "(@p" port)
     (for ([elt (in-vector (shen-tuple-args datum))])
       (write-string " " port)
       (shen-printer elt port))
     (write-string ")" port)]
    [(? vector?)
     (write-string "<" port)
     (unless (vector-empty? datum)
       (shen-printer (vector-ref datum 0) port)
       (for ([elt (in-vector datum 1)])
         (write-string " " port)
         (shen-printer elt port)))
     (write-string ">" port)]
    [#\|
     (write-string "bar!" port)]
    [_
     (write datum port)]))
