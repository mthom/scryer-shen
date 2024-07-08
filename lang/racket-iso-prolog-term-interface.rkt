#lang racket

(require brag/support
         "iso-prolog-bnf.rkt"
         (only-in "reader.rkt" shen-readtable))

(provide read-iso-prolog-term)

(define-lex-abbrev graphic-char
  (union "#" "$" "&" "*" "+" "-" "." "/" ":" "<" "=" ">" "?" "@" "^" "~"))

(define-lex-abbrev single-quoted-char
  (union "" "\"" "`" " " graphic-char alphabetic numeric solo-char))

(define-lex-abbrev single-quoted
  (concatenation "'" single-quoted-char "'"))

(define-lex-abbrev solo-char
  (union "!"  "("  ")"  " "  ";"  "["  "]"  "{"  "}"  "|" "%"))

(define-lex-abbrev atom
  (union (concatenation lower-case (repetition 0 +inf.0 (union alphabetic numeric "_")))
         (concatenation "'" (repetition 0 +inf.0 single-quoted-char) "'")
         ";"
         "!"
         "[]"
         "{}"
         graphic-char
         single-quoted))

(define-lex-abbrev variable
  (concatenation upper-case (repetition 0 +inf.0 (union alphabetic numeric))))

(define-lex-abbrev quoted-string
  (concatenation "\"" any-string "\""))

(define (make-tokenizer port)
  (define (next-token)
    (define iso-prolog-lexer
      (lexer
       ["|" (token 'BAR lexeme)]
       [atom (token 'ATOM (string->symbol lexeme))]
       [variable (token 'VARIABLE (string->symbol lexeme))]
       [numeric (token 'NUMBER (string->number lexeme))]
       [quoted-string (token 'STRING lexeme)]
       [whitespace (token 'WHITESPACE lexeme #:skip? #t)]
       [(char-set "()[],| ") lexeme]
       [(eof) (void)]))
    (iso-prolog-lexer port))
  next-token)

(define (read-iso-prolog-term port)
  (define line (read-line port))
  (iso-prolog-term->shen-expr (parse-to-datum (apply-tokenizer-maker make-tokenizer line))))

#|
(define (iso-prolog-atom->s-expr-atom iso-prolog-atom)
  (if (char-lower-case? (string-ref (symbol->string iso-prolog-atom) 0))
      (string->symbol (string-replace (symbol->string iso-prolog-atom) "_" "-"))
      iso-prolog-atom))
|#

#|
(define (s-expr->iso-prolog-term s-expr)
  (define output-port (open-output-string))

  (let loop ([s-expr s-expr])
    (match s-expr
      [(list 'quote (list* term terms ... tail))
       (write-string "[" output-port)
       (loop term)
       (for ([term (in-list terms)])
         (write-string ", " output-port)
         (loop term))
       (unless (empty? tail)
         (write-string " | " output-port)
         (loop tail))
       (write-string "]" output-port)]
      [(list (? symbol? atom) term terms ...)
       (write atom output-port)
       (write-string "(" output-port)
       (loop term)
       (for ([term (in-list terms)])
         (write-string ", " output-port)
         (loop term))
       (write-string ")" output-port)]
      [(or (? number?) (? symbol?) (? string?))
       (write s-expr output-port)]
      [(cons a d)
       (write-string "'.'(" output-port)
       (loop a)
       (write-string ", " output-port)
       (loop d)
       (write-string ")" output-port)]
      [empty
       (write-string "[]" output-port)]))

  (get-output-string output-port))
|#

(define (iso-prolog-term->shen-expr term-tree)
  (define output-port (open-output-string))

  (let write-shen-term ([term-tree term-tree])
    (match term-tree
      [(list 'term inner-term)
       (write-shen-term inner-term)]
      [(list 'atom '|[]|)
       (write-string "[]" output-port)]
      [(list (or 'atom 'variable 'number 'string) atom)
       (write atom output-port)]
      [(list 'clause (list 'atom '|'.'|) a d)
       (write-string "[" output-port)
       (write-shen-term a)
       (write-string " | " output-port)
       (write-shen-term d)
       (write-string "]" output-port)]
      [(list 'clause term terms ...)
       (write-string "(" output-port)
       (write-shen-term term)
       (for ([term (in-list terms)])
         (write-string " " output-port)
         (write-shen-term term))
       (write-string ")" output-port)]
      [(list 'list term terms ... "|" tail)
       (write-string "[" output-port)
       (write-shen-term term)
       (for ([term (in-list terms)])
         (write-string " " output-port)
         (write-shen-term term))
       (write-string " | " output-port)
       (write-shen-term tail)
       (write-string "]" output-port)]
      [(list 'list term terms ...)
       (write-string "[" output-port)
       (write-shen-term term)
       (for ([term (in-list terms)])
         (write-string " " output-port)
         (write-shen-term term))
       (write-string "]" output-port)]))

  (parameterize ([current-readtable shen-readtable])
    (read (open-input-string (get-output-string output-port)))))

;; (parse-to-datum (apply-tokenizer-maker make-tokenizer "'.'(number,'.'(false,[]))"))
;; (parse-to-datum (apply-tokenizer-maker make-tokenizer "term"))
;; (parse-to-datum (apply-tokenizer-maker make-tokenizer "term(b,c,[])"))
