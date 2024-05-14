SHELL := /bin/bash

.PHONY: \
	check-requirements \
	install \
	uninstall
	install-rust-toolchain \

help: ### Show available commands short description
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

check-requirements: ### Check if requirements are satisfied
	@. ./bin/check.sh && \
	check_requirements

install: check-requirements ### Install [scryer-shen with raco](https://github.com/mthom/scryer-shen/issues/2#issuecomment-2106059943)
	@. ./bin/install.sh && \

install-rust-toolchain: check-requirements ### Install [default rust toolchain](https://rustup.rs/)
	@. ./bin/install.sh && \
	install_rust_toolchain

	install

uninstall: check-requirements ### Uninstall [scryer-shen with raco](https://github.com/mthom/scryer-shen/issues/2#issuecomment-2106059943)
	@. ./bin/install.sh && \
	uninstall