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
           syntax/parse
           syntax/readerr
           syntax/strip-context
           syntax/stx)

  (provide detect-prolog-syntax
           shen-readtable)

  (define (wrapper1 t is-syntax?)
    (parameterize ([current-readtable shen-readtable])
      (if is-syntax?
          (strip-context (detect-prolog-syntax (expand-shen-form (t))))
          (syntax->datum (detect-prolog-syntax (expand-shen-form (datum->syntax #f (t))))))))

  (define (detect-prolog-syntax stx)
    (syntax-parse stx
      [((~datum defprolog) . body)
       (quasisyntax/loc stx
         (defprolog . #,(tag-prolog-functors #'body)))]
      [((~datum prolog?) . body)
       (quasisyntax/loc stx
         (prolog? . #,(tag-prolog-functors #'body)))]
      [(a . d)
       (quasisyntax/loc stx
         (#,(detect-prolog-syntax #'a)
          .
          #,(detect-prolog-syntax #'d)))]
      [_ stx]))

  (define (tag-prolog-functors stx)
    (syntax-parse stx
      [(id:id (~and (arg:expr ...+) brackets) . more)
       ;; 'paren-shape property being null affirms that round brackets were used
       (cond [(and (not (syntax-property #'brackets 'paren-shape))
                   (= (+ (syntax-position #'id) (syntax-span #'id))
                      (syntax-position #'brackets)))
              (let ([adjusted-more (tag-prolog-functors #'more)]
                    [arguments     (tag-prolog-functors #'(arg ...))])
                (datum->syntax
                 #f
                 `((#%prolog-functor ,#'id ,@arguments) . ,adjusted-more)
                 stx))]
             [else
              (with-syntax ([(_ . rest) stx])
                (let ([adjusted-rest (tag-prolog-functors #'rest)])
                  (datum->syntax #f
                                 `(,#'id . ,adjusted-rest)
                                 stx)))])]
      [(a:id . d)
       #:when (free-identifier=? #'a #'#%prolog-functor)
       (raise-syntax-error #f "#%prolog-functor is a reserved symbol in defprolog and prolog? contexts" stx)]
      [(a . d)
       (quasisyntax/loc stx
         (#,(tag-prolog-functors #'a)
          .
          #,(tag-prolog-functors #'d)))]
      [_ stx]))

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
                    #\; 'terminating-macro (const #\;)
                    ;; parse # like any other symbol char
                    #\# #\a (current-readtable)))

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
        term))
    (datum->syntax #f list-contents)))
