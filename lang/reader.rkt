(module reader syntax/module-reader
  #:language 'shen 
  #:wrapper1 (lambda (t)
               (parameterize ([current-readtable shen-readtable])
                 (t)))
  #:language-info '#(shen/lang/lang-info get-info #f)
  #:info (lambda (key defval default)
           (case key
             [(drracket:default-filters) '(["Shen Sources" "*.shen"])]
             [(drracket:default-extension) "Shen"]
             [(drracket:submit-predicate)
              (dynamic-require 'shen/tools/submit 'repl-submit?)]
             #|
             [(color-lexer)
              (dynamic-require 'recursive-language/tool/syntax-color 'get-syntax-token)]
             |#
             [else (default key defval)]))

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
                                   (yield '())
                                   (yield (void)))
                            ([#\|] (read-char in)
                                   (consume-spaces in)
                                   (let ([term (read in)])
                                     (consume-spaces in)
                                     (if (equal? (peek-char in) #\])
                                         (yield term)
                                         (let-values ([(line col pos) (port-next-location in)])
                                           (raise-read-error "expected a closing ']'" src line col pos 1))))
                                   (read-char in)
                                   (yield (void)))
                            (else (yield (read in))))
                          (loop)))]
                 #:break (void? term))
        term))
    (foldr (lambda (item list) (if (void? list) item `(cons ,item ,list)))
           (void)
           list-contents)))