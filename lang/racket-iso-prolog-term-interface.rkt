#lang racket

(require brag/support
         "iso-prolog-bnf.rkt"
         racket/generator
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

(define-lex-abbrev number
  (repetition 1 +inf.0 numeric))

(define-lex-abbrev variable
  (concatenation (union upper-case "_")
                 (repetition 0 +inf.0 (union alphabetic numeric))))

(define-lex-abbrev quoted-string
  (concatenation "\"" any-string "\""))

(define (make-tokenizer port)
  (define (next-token)
    (define iso-prolog-lexer
      (lexer
       ["|" (token 'BAR lexeme)]
       [atom (token 'ATOM (string->symbol lexeme))]
       [variable (token 'VARIABLE (string->symbol lexeme))]
       [number (token 'NUMBER (string->number lexeme))]
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
  (define (unquote-string string)
    (substring string 1 (sub1 (string-length string))))

  (match (symbol->string atom)
    [(and (var string) (or "-" "->" "-->")) string]
    [(and (var string) (regexp #rx"^'.*'$"))
     (unquote-string string)]
    [string (string-replace string "_" "-")]))

(define (iso-prolog-term->shen-expr term-tree)
  (define output-port (open-output-string))

  (define (subterms term)
    (for/list ([term (in-generator
                      (let loop ([term term])
                        (match term
                          [(list 'term term)
                           (loop term)]
                          [(list 'atom '|[]|)
                           (void)]
                          [(list 'clause
                                 (list 'atom '|'.'|)
                                 a d)
                           (yield a)
                           (loop d)]
                          [term (yield term)])))])
      term))

  (let write-shen-term ([term-tree term-tree])
    (match term-tree
      [(list 'term inner-term)
       (write-shen-term inner-term)]
      [(list (or 'atom 'variable) atom)
       (write-string (un-bar-bracket atom) output-port)]
      [(list (or 'number 'string) datum)
       (write datum output-port)]
      [(list 'clause (list 'atom '|'.'|)
             (list 'term (list 'atom '|'.'|))
             a d)
       (write-string "[" output-port)
       (write-shen-term a)
       (write-string " | " output-port)
       (write-shen-term d)
       (write-string "]" output-port)]
      [(and (var term)
            (list 'clause (list 'atom '|'.'|)
                  a d))
       (let ([subterms (subterms term)])
         (write-string "(" output-port)
         (write-shen-term (car subterms))
         (for ([subterm (in-list (cdr subterms))])
           (write-string " " output-port)
           (write-shen-term subterm))
         (write-string ")" output-port))]
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

;; (parse-to-datum (apply-tokenizer-maker make-tokenizer "type_check_error(inference_limit_exceeded)"))
;; (parse-to-datum (apply-tokenizer-maker make-tokenizer "'.'(number,'.'(false,[]))"))
;; (parse-to-datum (apply-tokenizer-maker make-tokenizer "term"))
;; (parse-to-datum (apply-tokenizer-maker make-tokenizer "term(b,c,[])"))
;; (parse-to-datum (apply-tokenizer-maker make-tokenizer "'.'('.'(type_functor,'.'('.'('h-list','.'('.'('.',number,'.'('.',symbol,[])),[])),[])),[])"))
