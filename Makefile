CASK ?= cask
EMACS ?= emacs

SRCS = .cask kubernetes.el kubernetes-evil.el
PACKAGE_FILE = kubernetes.el

VERSION := $(shell EMACS=${EMACS} ${CASK} version)
TAR     := dist/kubernetes-$(VERSION).tar


.PHONY: build dist install help test clean clean-all release \
	set-package-version git-release assert-on-master assert-clean-worktree


build : $(SRCS)
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
	${CASK} exec ${EMACS} -Q --batch -l package \
		--eval "(add-to-list 'package-archives '(\"MELPA Stable\" . \"https://stable.melpa.org/packages/\"))" \
		-f package-initialize \
		--eval "(unless package-archive-contents (package-refresh-contents))" \
		--eval "(package-install-file \"$(TAR)\")"


test : $(SRCS)
	${CASK} exec ert-runner


clean :
	${CASK} clean-elc
	rm -rf dist


clean-all: clean
	rm -rf .cask "~/.emacs.d/elpa/kubernetes-$(VERSION)"


release : assert-clean-worktree assert-on-master clean set-package-version dist git-release
	@echo 'Release successful.'


assert-on-master :
	@if [ "$$(git rev-parse --abbrev-ref HEAD)" != master ]; then \
		echo "Attempting to deploy non-master branch to production."; exit 1; \
	fi

assert-clean-worktree :
	@if ! git diff-index --quiet HEAD -- ; then \
		echo "Work tree is dirty. Aborting release."; exit 1; \
	fi


set-package-version :
	@read -p "Enter the next version (currently at $(VERSION)): " NEXT && \
	if ! echo "$${NEXT}" | grep -Eq '[0-9]+\.[0-9]+\.[0-9]+'; then \
		echo 'Must supply a semver tag, e.g. 1.2.3'; exit 1; \
	fi && \
	sed -i.bak "s/^;; Version:[^\n]*/;; Version: $${NEXT}/" "$(PACKAGE_FILE)"
	@rm "$(PACKAGE_FILE).bak"


git-release :
	@git add "$(PACKAGE_FILE)"
	@export TAG="$$(EMACS=${EMACS} ${CASK} version)"; \
	git commit --quiet -m "Release $${TAG}" && \
	git tag "$${TAG}" && \
	while true; do \
		read -p "Push to GitHub? [yN]: " CHOICE; \
		case $$CHOICE in \
			[Nn]|"") echo "Remember to push the tag with your changes."; exit;; \
			[Yy]   ) git push --quiet origin master "$${TAG}"; break;; \
		esac \
	done

.cask :
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
