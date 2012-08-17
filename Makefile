.PHONY: test

test:
	emacs -Q -L . -l test/popwin-test.el
