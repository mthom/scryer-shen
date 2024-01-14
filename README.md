An experimental, very much in-progress implementation of the Shen
programming language in Racket.

Unlike most other implementations of Shen, scryer-shen implements Shen
directly in its host languages without bootstrapping it from a seed
implementation of KLambda.  The intention is to facilitate direct
integration with [Scryer Prolog](http://github.com/mthom/scryer-prolog),
a performant and feature-rich ISO Prolog system with powerful
metaprogramming capabilities. More details to come.

scryer-shen can be run in the Racket REPL (for
`#lang shen` modules) or compiled to an executable using

```
raco exe --cs -o shen ++lib racket/lang/reader repl.rkt
```

For this branch, the scryer-prolog executable needs to be copied to
the install directory `dist/bin`. It can be downloaded from the above
link and built by following the instructions there.
