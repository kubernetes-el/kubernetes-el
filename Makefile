CASK ?= cask
EMACS ?= emacs
EMACS_BATCH = ${CASK} exec ${EMACS} -Q --batch -l package

REPO = github.com/chrisbarrett/kubernetes-el

DEPS_SCRIPT = assets/project-deps.el
DEPS_PNG = assets/project-deps.png

CASKDIR = .cask
SRCS = $(wildcard *.el)
TARGETS = $(SRCS:.el=.elc)

MAIN_PACKAGE_FILE = kubernetes.el
EVIL_PACKAGE_FILE = kubernetes-evil.el

VERSION := $(shell EMACS=${EMACS} ${CASK} version)
TAR     := dist/kubernetes-$(VERSION).tar



.PHONY: build dist install help test clean clean-all release \
	set-package-version assert-on-master assert-clean-worktree \
	git-release github-browse-release


build : compile $(DEPS_PNG)

compile: $(SRCS) $(CASKDIR)
	! (${CASK} eval "(let ((byte-compile-error-on-warn t)) (cask-cli/build))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; ${CASK} clean-elc && exit $$ret)


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


release : assert-clean-worktree assert-on-master clean test set-package-version dist git-release github-browse-release
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
	sed -i.bak "s/^;; Version:[^\n]*/;; Version: $${NEXT}/" "$(MAIN_PACKAGE_FILE)" && \
	sed -i.bak "s/^;; Version:[^\n]*/;; Version: $${NEXT}/" "$(EVIL_PACKAGE_FILE)" && \
	sed -i.bak "s/^;; Package-Requires:[^)]*)/;; Package-Requires: ((kubernetes \"$${NEXT}\")/" "$(EVIL_PACKAGE_FILE)"

	@rm "$(MAIN_PACKAGE_FILE).bak"
	@rm "$(EVIL_PACKAGE_FILE).bak"


git-release :
	@git add "$(MAIN_PACKAGE_FILE)"
	@git add "$(EVIL_PACKAGE_FILE)"
	@export TAG="$$(EMACS=${EMACS} ${CASK} version)"; \
	git commit --quiet -m "Release $${TAG}" && \
	git tag "$${TAG}" && \
	while true; do \
		read -p "Push to GitHub? [Yn]: " CHOICE; \
		case $$CHOICE in \
			[Yy]|"") git push --quiet origin master "$${TAG}"; break;; \
			[Nn]   ) echo "Remember to push the tag with your changes."; exit;; \
		esac \
	done


github-browse-release :
	@export TAG="$$(git describe --abbrev=0 --tags)"; \
	python -mwebbrowser "https://$(REPO)/releases/tag/$$TAG"


$(CASKDIR) :
	${CASK} install


$(DEPS_PNG) : $(DEPS_SCRIPT) $(SRCS)
	$(EMACS_BATCH) -f package-initialize -l $(DEPS_SCRIPT) -f project-deps-generate

.PHONY: autoloads
autoloads:
	$(EMACS) -Q --batch --eval "(package-initialize)" --eval "(package-generate-autoloads \"kubernetes\" \".\")"



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
