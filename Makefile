.PHONY: test
test:
	cask exec ert-runner -L $(CURDIR)
