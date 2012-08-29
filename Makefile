.PHONY: test

test:
	emacs -Q -L . -l test/popwin-test.el

test-nw:
	emacs -Q -nw -L . -l test/popwin-test.el
