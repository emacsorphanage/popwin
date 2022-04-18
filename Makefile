SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

TEST-FILES := $(shell ls test/popwin-*.el)

.PHONY: clean checkdoc lint package install compile

ci: clean package install compile test

package:
	@echo "Packaging..."
	$(EASK) package

install:
	@echo "Installing..."
	$(EASK) install --dev

compile:
	@echo "Compiling..."
	$(EASK) compile

test:
	@echo "Testing..."
	$(EASK) exec ert-runner -L . $(LOAD-TEST-FILES) -t '!no-win' -t '!org'

clean:
	rm -rf .eask *.elc
