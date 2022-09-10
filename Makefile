EMACS ?= emacs
EASK ?= eask

.PHONY: clean checkdoc lint package install compile test warnings

ci: clean package install compile

package:
	@echo "Packaging..."
	$(EASK) package

install:
	@echo "Installing..."
	$(EASK) install

compile:
	@echo "Compiling..."
	$(EASK) compile

test:
	@echo "Testing..."
	$(EASK) test ert ./test/*.el

checkdoc:
	@echo "Run checkdoc..."
	$(EASK) lint checkdoc

lint:
	@echo "Run package-lint..."
	$(EASK) lint package

clean:
	$(EASK) clean-all

warnings:
	@emacs -q -batch \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile \
	  popwin.el
