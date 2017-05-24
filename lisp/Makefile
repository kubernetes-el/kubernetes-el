CASK ?= cask
EMACS ?= emacs
EMACS_BATCH = ${CASK} exec ${EMACS} -Q --batch -l package

CASKDIR = .cask
SRCS = $(wildcard *.el)
TARGETS = $(SRCS:.el=.elc)

MAIN_PACKAGE_FILE = kubernetes.el
EVIL_PACKAGE_FILE = kubernetes-evil.el

VERSION := $(shell EMACS=${EMACS} ${CASK} version)
TAR     := dist/kubernetes-$(VERSION).tar



.PHONY: build dist install help test clean clean-all release \
	set-package-version git-release


build : $(TARGETS)

$(TARGETS) : $(SRCS) $(CASKDIR)
	${CASK} clean-elc
	${CASK} build


dist : $(TAR)


$(TAR) : $(SRCS)
	${CASK} package


help :
	@echo 'Makefile for kubernetes-el'
	@echo
	@echo 'Main tasks:'
	@echo
	@echo '  build (default) Compile Lisp files.'
	@echo '  help            Show this usage information.'
	@echo '  install         Install kubernetes.el using the Emacs package manager.'
	@echo '  test            Run automated test suites.'
	@echo '  release         Prepare for a GitHub release.'
	@echo '  clean           Delete generated output files.'
	@echo '  clean-all       Like clean, but also delete vendored local dependencies and the installed package.'


install : $(TAR)
	$(EMACS_BATCH) \
		--eval "(add-to-list 'package-archives '(\"MELPA Stable\" . \"https://stable.melpa.org/packages/\"))" \
		-f package-initialize \
		--eval "(unless package-archive-contents (package-refresh-contents))" \
		--eval "(package-install-file \"$(TAR)\")"


test : $(SRCS)
	${CASK} clean-elc
	${CASK} exec ert-runner


clean :
	${CASK} clean-elc
	rm -rf dist


clean-all: clean
	rm -rf $(CASKDIR) "~/.emacs.d/elpa/kubernetes-$(VERSION)"


release : clean test set-package-version dist


set-package-version :
	@read -p "Enter the next version (currently at $(VERSION)): " NEXT && \
	if ! echo "$${NEXT}" | grep -Eq '[0-9]+\.[0-9]+\.[0-9]+'; then \
		echo 'Must supply a semver tag, e.g. 1.2.3'; exit 1; \
	fi && \
	sed -i.bak "s/^;; Version:[^\n]*/;; Version: $${NEXT}/" "$(MAIN_PACKAGE_FILE)" && \
	sed -i.bak "s/^;; Version:[^\n]*/;; Version: $${NEXT}/" "$(EVIL_PACKAGE_FILE)" && \
	sed -i.bak "s/^;; Package-Requires:[^)]*)/;; Package-Requires: ((kubernetes \"$${NEXT}\")/" "$(EVIL_PACKAGE_FILE)"

	@rm "$(MAIN_PACKAGE_FILE).bak"
	@rm "$(EVIL_PACKAGE_FILE).bak"


git-release :
	@git add "$(MAIN_PACKAGE_FILE)"
	@git add "$(EVIL_PACKAGE_FILE)"
	@export TAG="$$(EMACS=${EMACS} ${CASK} version)"; \
	git commit --quiet -m "Release $${TAG}" && git tag "$${TAG}"


$(CASKDIR) :
	${CASK} install


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
