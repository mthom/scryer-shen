#lang racket

(require "namespaces.rkt"
         racket/syntax
         "syntax-utils.rkt"
         syntax/parse
         syntax/stx
         "systemf.rkt")

(provide unpackage-shen-package)

(define (eval-export-list export-list)
  (define quoted-export-list (quote-pattern export-list))
  (if (stx-null? quoted-export-list)
      '()
      (eval-syntax quoted-export-list shen-namespace)))

(define (unpackage-shen-package pkg-name export-list top-level-decls)
  (let ([export-list (eval-export-list export-list)]
        [external-symbols (make-hasheq)]
        [internal-symbols (make-hasheq)])
    (values (map (curry fqn pkg-name export-list external-symbols internal-symbols)
                 (syntax->list top-level-decls))
            external-symbols
            internal-symbols)))

(define (fqn pkg-name export-list external-symbols internal-symbols top-level-decl)
  (letrec ([fqn (lambda (top-level-decl)
                  (syntax-parse top-level-decl
                    [decl:shen-var-id
                     #'decl]
                    [decl:id
                     (cond [(member (syntax->datum #'decl) (reserved-shen-symbols))
                            #'decl]
                           [(member (syntax->datum #'decl) export-list)
                            (hash-set! external-symbols #'decl (void))
                            #'decl]
                           [(let ([pkg-name-str (symbol->string (syntax->datum pkg-name))]
                                  [decl-str (symbol->string (syntax->datum #'decl))])
                              (or (string-prefix? decl-str "shen.")
                                  (string-prefix? decl-str (string-append pkg-name-str "."))))
                            (hash-set! internal-symbols #'decl (void))
                            #'decl]
                           [else
                            (let ([dotted-id (format-id top-level-decl "~a.~a" pkg-name #'decl)])
                              (hash-set! internal-symbols dotted-id (void))
                              dotted-id)])]
                    [decl:expr
                     #:when (stx-pair? #'decl)
                     (quasisyntax/loc top-level-decl
                       (#,(fqn (stx-car #'decl))
                        .
                        #,(stx-map fqn (stx-cdr #'decl))))]
                    [decl:expr
                     #'decl]))])
    (fqn top-level-decl)))
