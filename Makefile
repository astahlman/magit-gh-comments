 test:
	cask emacs -q \
		-L $(CURDIR) \
		-l test-magit-gh-comments.el \
		-f ert-run-tests-batch-and-exit
