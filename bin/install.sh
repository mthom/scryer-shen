#!/usr/bin/env bash
set -Eeu

# Install scryer-shen executable
function install {
  raco setup
  ( cd ../ && raco pkg install ./scryer-shen )
  raco exe --cs -o shen ++lib racket/lang/reader repl.rkt
}

# Uninstall scryer-shen executable
function uninstall {
  ( cd ../ && raco pkg remove scryer-shen )
}

set +Eeu
