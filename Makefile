CASK ?= cask
EMACS ?= emacs
EMACS_BATCH = ${CASK} exec ${EMACS} -Q --batch -l package

REPO = github.com/kubernetes-el/kubernetes-el

DEPS_SCRIPT = docs/assets/project-deps.el
DEPS_PNG = docs/assets/project-deps.png

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


#: Compile Lisp files
build : compile $(DEPS_PNG)

compile: $(SRCS) $(CASKDIR)
	! (${CASK} eval "(cask-cli/build)" | tee 2>&1 | egrep -a "(Error):") ; (ret=$$? ; ${CASK} clean-elc && exit $$ret)

dist : $(TAR)

$(TAR) : $(SRCS)
	${CASK} package

# Install kubernetes.el using the Emacs package manager
install : $(TAR)
	$(EMACS_BATCH) \
		--eval "(add-to-list 'package-archives '(\"MELPA Stable\" . \"https://stable.melpa.org/packages/\"))" \
		-f package-initialize \
		--eval "(unless package-archive-contents (package-refresh-contents))" \
		--eval "(package-install-file \"$(TAR)\")"


#: Run all static tests
test-static:
	pre-commit run --all-files

#: Run all unit tests
test : $(SRCS)
	${CASK} clean-elc
	${CASK} exec ert-runner --reporter ert+duration
	${CASK} exec buttercup -L . tests/


#: Delete generated output files
clean :
	${CASK} clean-elc
	rm -rf dist

#: Like clean, but also delete vendored local dependencies and the installed package
clean-all: clean
	rm -rf $(CASKDIR) "~/.emacs.d/elpa/kubernetes-$(VERSION)"


#: Release a new version of the package
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
