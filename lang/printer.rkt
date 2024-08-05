#lang racket

(require "pairs.rkt"
         racket/base
         racket/generator
         racket/match)

(provide shen-printer
         type-printer)

(define (print-cons-contents args port printer)
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
    (printer arg port)))

(define/contract (type-printer datum port)
  (any/c output-port? . -> . any)
  (match datum
    [(list 'cons a d)
     (write-char #\[ port)
     (print-cons-contents (cons a d) port type-printer)
     (write-char #\] port)]
    [(list '--> a d)
     (type-printer a port)
     (write-string " --> " port)
     (type-printer d port)]
    [(list terms ...)
     (write-char #\( port)
     (print-cons-contents terms port type-printer)
     (write-char #\) port)]
    [(? empty?)
     (write-string "[]" port)]
    [(? shen-tuple?)
     (write-string "(@p" port)
     (for ([elt (in-vector (shen-tuple-args datum))])
       (write-string " " port)
       (type-printer elt port))
     (write-string ")" port)]
    [(? vector?)
     (write-string "<" port)
     (unless (vector-empty? datum)
       (type-printer (vector-ref datum 0) port)
       (for ([elt (in-vector datum 1)])
         (write-string " " port)
         (type-printer elt port)))
     (write-string ">" port)]
    [_ (shen-printer datum port)]))

(define/contract (shen-printer datum port)
  (any/c output-port? . -> . any)
  (match datum
    [(? empty?)
     (write-string "[]" port)]
    [(? cons?)
     (write-char #\[ port)
     (print-cons-contents datum port shen-printer)
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
    [(? procedure?)
     (write (object-name datum) port)]
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
