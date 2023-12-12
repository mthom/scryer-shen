#lang racket

(require racket/base
         racket/generator
         racket/match)

(provide shen-printer)

(define (print-cons-contents args port)
  (for ([arg (in-generator
              (let loop ([pair args])
                (cond [(pair? pair)
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
    [(list args ...)
     (write-char #\[ port)
     ;; don't print a space before the first element but do print one
     ;; before every subsequent element
     (print-cons-contents args port)
     (write-char #\] port)]
    [(cons hd tl)
     (write-char #\[ port)
     (print-cons-contents (cons hd tl) port)
     (write-char #\] port)]
    [(? symbol?)
     (write-string (symbol->string datum) port)]
    [(? string?)
     (write-string datum port)]
    [(? boolean?)
     (write-string (if datum "true" "false") port)]
    [_
     (write datum port)]))
