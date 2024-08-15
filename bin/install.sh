#!/usr/bin/env bash
set -Eeu

# Install `scryer-shen` executable
function install {
  if [ -e shen ];
  then
    printf '%s.%s' 'shen executable exists already.' $'\n'

    return 0
  fi

  local raco_executable
  raco_executable=$(which raco)

  "${raco_executable}" setup --clean
  "${raco_executable}" setup

  ( cd ../ && \
    "${raco_executable}" pkg install ./scryer-shen )

  ( "${raco_executable}" exe --cs -o shen ++lib racket/lang/reader repl.rkt || \
    "${raco_executable}" pkg show scryer-shen )
}

# ⚠️ Requires [`curl`](https://curl.se/download.html)
function install_rust_toolchain {
  curl \
    --proto '=https' \
    --tlsv1.2 \
    -sSf https://sh.rustup.rs \
    -o install-rust-toolchain.sh && \
    sh -c '( chmod u+x ./install-rust-toolchain.sh && ./install-rust-toolchain.sh -y )'
}

# Uninstall `scryer-shen` executable
function uninstall {
  ( cd ../ && \
    raco pkg remove scryer-shen )
}

set +Eeu
