CASK = cask
EMACS = emacs

SRCS = .cask kubernetes.el kubernetes-evil.el

VERSION = $(shell EMACS=$(EMACS) $(CASK) version)
TAR     = dist/kubernetes-$(VERSION).tar


.PHONY: build dist install help test clean clean-all


build : $(SRCS)
	$(CASK) build


dist : $(TAR)


$(TAR) : $(SRCS)
	$(CASK) package


help :
	@echo 'Makefile for kubernetes-el'
	@echo
	@echo 'Available tasks:'
	@echo
	@echo '  build (default) Compile Lisp files.'
	@echo '  help            Show this usage information.'
	@echo '  install         Install kubernetes.el using the Emacs package manager.'
	@echo '  test            Run automated test suites.'
	@echo '  clean           Delete generated output files.'
	@echo '  clean-all       Like clean, but also delete vendored local dependencies and the installed package.'


install : $(TAR)
	$(CASK) exec $(EMACS) -Q --batch -l package \
		--eval "(add-to-list 'package-archives '(\"MELPA Stable\" . \"https://stable.melpa.org/packages/\"))" \
		-f package-initialize \
		--eval "(unless package-archive-contents (package-refresh-contents))" \
		--eval "(package-install-file \"$(TAR)\")"


test : $(SRCS)
	$(CASK) exec ert-runner


clean :
	$(CASK) clean-elc


clean-all: clean
	rm -rf .cask "~/.emacs.d/elpa/kubernetes-$(VERSION)"


.cask :
	$(CASK) install


# Assert cask is installed

ifeq (, $(shell which cask))

define MESSAGE
Building this project requires Cask.

	https://github.com/cask/cask

macOS:

	brew install cask

*Nix:

	curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

endef

$(error $(MESSAGE))
endif
