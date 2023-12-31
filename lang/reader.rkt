(module reader syntax/module-reader
  #:language 'shen
  #:language-info '#(shen/lang/lang-info get-info #f)
  #:wrapper1 (lambda (t)
               (parameterize ([current-readtable shen-readtable])
                 (t)))
  #:info (lambda (key defval default)
           (case key
             [(drracket:default-filters) '(["Shen Sources" "*.shen"])]
             [(drracket:default-extension) "Shen"]
             [(drracket:submit-predicate)
              (dynamic-require 'shen/tools/submit 'repl-submit?)]
             [(color-lexer)
              (dynamic-require 'shen/tools/colorer 'shen-colorer)]
             [else (default key defval)]))

  (require (only-in racket
                    const
                    [read r:read]
                    [read-syntax r:read-syntax])
           racket/generator
           syntax/readerr
           "shen-cons.rkt")

  (provide shen-readtable)

  (define (shen-cons-fold list-items)
    (foldr (lambda (item acc) (if (void? acc) item (datum->syntax #f (list #'shen-cons item acc))))
           (void)
           (syntax->list list-items)))

  (define (read-list)
    (define outer-scope (make-syntax-introducer #t))
    (case-lambda
      [(ch in)
       (define inner-scope (make-syntax-introducer))
       (syntax->datum
        (outer-scope
         (inner-scope
          (shen-cons-fold (inner-scope (outer-scope (read-list-items in (object-name in))))))))]
      [(ch in src line col pos)
       (define inner-scope (make-syntax-introducer))
       (outer-scope
        (inner-scope
         (shen-cons-fold (inner-scope (outer-scope (read-list-items in src))))))]))

  (define shen-readtable
    (make-readtable #f
                    #\[ 'terminating-macro (read-list)
                    #\| 'terminating-macro (const #\|)))

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
