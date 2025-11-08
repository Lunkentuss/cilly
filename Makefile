.PHONY: build
build:
	nix build .

.PHONY: lint
lint:
	@echo Running hlint
	@hlint src/*
	@hlint helper/*
	@hlint test_snapshot/*

.PHONY: cilly.cabal
cilly.cabal:
	hpack

.PHONY: test-snapshot-check
test-snapshot-check:
	runghc test_snapshot/main.hs check

.PHONY: test-snapshot-gen
test-snapshot-gen:
	runghc test_snapshot/main.hs gen
