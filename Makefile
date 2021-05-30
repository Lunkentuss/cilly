.PHONY: build
build:
	nix build .

.PHONY: lint
lint:
	@echo Running hlint
	@hlint src/*
	@hlint helper/*
