#lang racket

(require "pairs.rkt"
         "vectors.rkt"
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

(define (print-vector-from-offset datum port [offset 0])
  (write-string "<" port)
  (unless (vector-empty? datum)
    (shen-printer (vector-ref datum offset) port)
    (for ([elt (in-vector datum (add1 offset))])
      (write-string " " port)
      (shen-printer elt port)))
  (write-string ">" port))

(define/contract (shen-printer datum port)
  (any/c output-port? . -> . any)
  (match datum
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
     (print-vector-from-offset datum port)]
    [(? vector-view?)
     (print-vector-from-offset (vector-view-vec datum) port (vector-view-offset datum))]
    [#\|
     (write-string "bar!" port)]
    [_
     (write datum port)]))
