#!/usr/bin/env bash
set -Eeu

# Install scryer-shen executable
function install {
  raco setup
  ( cd ../ && raco pkg install ./scryer-shen )
  raco exe --cs -o shen ++lib racket/lang/reader repl.rkt


# ⚠️ [`curl`](https://curl.se/download.html) is required from some unix-compatible host
function install_rust_toolchain {
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs -o install-rust-toolchain.sh
  sh -c '( chmod u+x ./install-rust-toolchain.sh && ./install-rust-toolchain.sh -y )'
}

# Uninstall scryer-shen executable
function uninstall {
  ( cd ../ && raco pkg remove scryer-shen )
}

set +Eeu
