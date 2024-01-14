(module reader syntax/module-reader
  #:language 'shen
  #:language-info '#(shen/lang/lang-info get-info #f)
  #:wrapper1 wrapper1
  #:info (lambda (key defval default)
           (case key
             [(drracket:default-filters) '(["Shen Sources" "*.shen"])]
             [(drracket:default-extension) "Shen"]
             [(drracket:submit-predicate)
              (dynamic-require 'shen/tools/submit 'repl-submit?)]
             [(color-lexer)
              (dynamic-require 'shen/tools/colorer 'shen-colorer)]
             [else (default key defval)]))

  (require "macros.rkt"
           racket
           racket/generator
           syntax/readerr
           syntax/strip-context
           syntax/stx)

  (provide shen-readtable)

  (define (wrapper1 t is-syntax?)
    (parameterize ([current-readtable shen-readtable])
      (if is-syntax?
          (strip-context (expand-shen-form (t)))
          (syntax->datum (expand-shen-form (datum->syntax #f (t)))))))

  (define (shen-cons-fold list-items)
    (foldr (lambda (item acc) (if (void? acc) item #`(cons #,item #,acc)))
           (void)
           (syntax->list list-items)))

  (define (read-list)
    (case-lambda
      [(ch in)
       (syntax->datum
        (shen-cons-fold (read-list-items in (object-name in))))]
      [(ch in src line col pos)
       (shen-cons-fold (read-list-items in src))]))

  (define shen-readtable
    (make-readtable #f
                    #\[ 'terminating-macro (read-list)
                    #\| 'terminating-macro (const #\|)
                    #\; 'terminating-macro (const #\;)))

  (define (consume-spaces in)
    (define ch (peek-char in))
    (when (and (char? ch) (char-whitespace? ch))
      (read-char in)
      (consume-spaces in)))

  (define (read-list-items in [src #f])
    (datum->syntax #f
                   (for/list ([term (in-generator
                                     (let loop ()
                                       (consume-spaces in)
                                       (case (peek-char in)
                                         ([#\]] (read-char in)
                                                (yield (datum->syntax #f '()))
                                                (yield (void)))
                                         ([#\|] (read-char in)
                                                (consume-spaces in)
                                                (let ([term (read-syntax/recursive src in)])
                                                  (consume-spaces in)
                                                  (if (equal? (peek-char in) #\])
                                                      (yield term)
                                                      (let-values ([(line col pos) (port-next-location in)])
                                                        (raise-read-error "expected a closing ']'" src line col pos 1))))
                                                (read-char in)
                                                (yield (void)))
                                         (else (yield (read-syntax/recursive src in))))
                                       (loop)))]
                              #:break (void? term))
                     term))))
