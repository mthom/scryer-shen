#lang racket

(require racket
         racket/generator
         syntax/readerr)

(provide shen-readtable)

(define read-list
  (case-lambda
    [(ch in)
     (read-list-items in (object-name in))]
    [(ch in src line col pos)
     (read-list-items in src)]))

(define shen-readtable
  (make-readtable #f
                  #\[ 'terminating-macro read-list
                  ;; parse #\| like an ordinary character.
                  #\| #\a #f))

(define (consume-spaces in)
  (define ch (peek-char in))
  (when (and (char? ch) (char-whitespace? ch))
    (read-char in)
    (consume-spaces in)))

(define (read-list-items in [src #f])
  (define list-contents
    (for/list ([term (in-generator
                      (let loop ()
                        (consume-spaces in)
                        (case (peek-char in)
                          ([#\]] (read-char in)
                                 (yield 'empty)                                      
                                 (yield (void)))
                          ([#\|] (read-char in)
                                 (consume-spaces in)
                                 (let ([term (read in)])
                                   (consume-spaces in)
                                   (if (equal? (peek-char in) #\])
                                       (yield term)
                                       (let-values ([(line col pos) (port-next-location in)])
                                         (raise-read-error "expected a closing ']'" src line col pos 1))))
                                 (yield (void)))
                          (else (yield (read in))))
                        (loop)))]
               #:break (void? term))
      term))
  (foldr (lambda (item list) (if (void? list) item `(cons ,item ,list)))
         (void)
         list-contents))