EMACS ?= emacs
ELPA_DIR ?= $(HOME)/.emacs.d/elpa
export ELPA_DIR

.PHONY: test compile lint
test:
	$(EMACS) -Q --batch -l tools/check.el -f package-check-test
compile:
	$(EMACS) -Q --batch -l tools/check.el -f package-check-compile
lint:
	$(EMACS) -Q --batch -l tools/check.el -f package-check-lint
