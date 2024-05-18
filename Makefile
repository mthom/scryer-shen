SHELL := /bin/bash

.PHONY: \
	build-scryer-prolog \
	check-requirements \
	clone-scryer-prolog-repository \
	clone-scryer-shen-repository \
	ensure-scryer-prolog-availability \
	ensure-scryer-shen-availability \
	install \
	install-rust-toolchain \
	uninstall \

help: ### Show available commands short description
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

check-requirements: ### Check if requirements are satisfied
	@. ./bin/check.sh && \
	check_requirements

clone-scryer-prolog-repository: ### Clone scryer-prolog repository
	test -d scryer-prolog || ( cd dist && git clone https://github.com/mthom/scryer-prolog )

clone-scryer-shen-repository: ### Clone scryer-shen repository
	test -d scryer-shen || git clone https://github.com/mthom/scryer-shen

install: check-requirements ### Install [scryer-shen with raco](https://github.com/mthom/scryer-shen/issues/2#issuecomment-2106059943)
	@. ./bin/install.sh && \
	install

install-rust-toolchain: check-requirements ### Install [default rust toolchain](https://rustup.rs/)
	@. ./bin/install.sh && \
	install_rust_toolchain

build-scryer-prolog: install-rust-toolchain clone-scryer-prolog-repository ### Build scryer-prolog executable
	( cd dist/scryer-prolog && \
	$$HOME/.cargo/bin/cargo build --release )

ensure-scryer-prolog-executable-availability: build-scryer-prolog ### Ensure Scryer prolog availability in ./dist/bin
	mkdir -p dist/bin && \
	mv ./dist/scryer-prolog/target/release/scryer-prolog ./dist/bin

ensure-scryer-shen-executable-availability: ensure-scryer-prolog-executable-availability ### Ensure Scryer shen availability in ./
	make install

uninstall: check-requirements ### Uninstall [scryer-shen with raco](https://github.com/mthom/scryer-shen/issues/2#issuecomment-2106059943)
	@. ./bin/install.sh && \
	uninstall