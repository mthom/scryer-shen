#lang racket

(require "namespaces.rkt"
         racket/syntax
         "syntax-utils.rkt"
         syntax/parse
         syntax/stx
         "systemf.rkt")

(provide add-internal-symbols-to-package!
         add-external-symbols-to-package!
         eval-export-list
         package-list
         unpackage-shen-package)

(define shen-packages (make-hasheq))

(define (package-list pkg-name [type 'external])
  (let ([package-ht (hash-ref! shen-packages pkg-name (thunk (make-hasheq)))])
    (for/list ([(symbol symbol-type) (in-hash package-ht)]
               #:when (eq? symbol-type type))
      symbol)))

(define (add-internal-symbols-to-package! pkg-name internal-symbols-list)
  (let ([package-ht (hash-ref! shen-packages pkg-name (thunk (make-hasheq)))])
    (map (lambda (symbol)
           (hash-set! package-ht symbol 'internal))
         internal-symbols-list)))

(define (add-external-symbols-to-package! pkg-name external-symbols-list)
  (let ([package-ht (hash-ref! shen-packages pkg-name (thunk (make-hasheq)))])
    (map (lambda (symbol)
           (hash-set! package-ht symbol 'external))
         external-symbols-list)))

(define (eval-export-list export-list)
  (parameterize ([current-namespace shen-namespace])
    (define quoted-export-list (expand (quote-pattern export-list)))
    (if (stx-null? quoted-export-list)
        '()
        (eval-syntax quoted-export-list))))

(define (unpackage-shen-package pkg-name export-list top-level-decls)
  (let ([external-symbols (make-hasheq)]
        [internal-symbols (make-hasheq)])
    (values (map (curry fqn pkg-name export-list external-symbols internal-symbols)
                 (syntax->list top-level-decls))
            external-symbols
            internal-symbols)))

(define (fqn pkg-name export-list external-symbols internal-symbols top-level-decl)
  (let fqn ([top-level-decl top-level-decl])
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
       #'decl])))
