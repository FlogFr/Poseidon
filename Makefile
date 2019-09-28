SHELL := /bin/bash

.PHONY: test
test:
	cabal new-test --enable-tests

.PHONY: sdist
sdist:
	cabal new-sdist

.PHONY: build
build:
	cabal new-build

.PHONY: doc
doc:
	cabal new-haddock poseidon
