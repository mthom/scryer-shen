#lang racket

(require racket/match
         (only-in shen/lang/reader shen-readtable)
         syntax-color/racket-lexer)

(provide shen-colorer)

(define (shen-colorer port)
  (match (peek-char port)
    [#\|
     (define-values (_line _col pos) (port-next-location port))
     (read-char port)
     (values "|" 'symbol #f pos (add1 pos))]
    [_
     (define-values (matching-text type paren-type start-loc end-loc)       
       (racket-lexer port))
     (match matching-text
       [(? eof-object?) (values matching-text 'eof #f #f #f)]
       [else
        (match-define (list cat paren)
       (match type
         ['error '(error #f)]
         ['comment '(comment #f)]
         ['sexp-comment '(comment #f)]
         ['white-space '(no-color #f)]
         ['constant '(constant #f)]
         ['string '(string #f)]
         ['no-color '(no-color #f)]
         ['parenthesis (list 'parenthesis paren-type)]
         ['hash-colon-keyword '(symbol #f)]
         ['symbol '(symbol #f)]
         ['eof '(eof #f)]
         ['other '(other #f)]))
        (values matching-text cat paren start-loc end-loc)])]))

#|
(define (shen-colorer port)
  (match (peek-char port)
    [(or #\[ #\] #\( #\) #\|) 'parenthesis]
    [(or (? char-lower-case?) #\@) 'symbol]
    [#\" 'string]
    [_ 'other]))
|#