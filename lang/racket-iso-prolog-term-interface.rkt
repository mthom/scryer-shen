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
         (repetition 1 +inf.0 graphic-char)
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

(define (un-bar-bracket atom)
  (match (symbol->string atom)
    [(and (var string) (or "-" "->" "-->")) string]
    [(regexp #rx"^|(.)*|$" (list "|" string "|"))
     (un-bar-bracket (string->symbol string))]
    [string (string-replace string "_" "-")]))

(define (iso-prolog-term->shen-expr term-tree)
  (define output-port (open-output-string))

  (let write-shen-term ([term-tree term-tree])
    (match term-tree
      [(list 'term inner-term)
       (write-shen-term inner-term)]
      [(list 'atom atom)
       (write-string (un-bar-bracket atom) output-port)]
      [(list (or 'variable 'number 'string) atom)
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
