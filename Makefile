.PHONY: build install help test clean release \
	assert-clean-worktree assert-on-master \
	lisp-release github-browse-release


build :
	$(MAKE) -C go build
	$(MAKE) -C lisp build


help :
	@echo 'Makefile for kubernetes-el'
	@echo
	@echo 'Main tasks:'
	@echo
	@echo '  build (default) Compile all targets in subprojects.'
	@echo '  help            Show this usage information.'
	@echo '  install         Install kubernetes.el using the Emacs package manager.'
	@echo '  test            Run automated test suites.'
	@echo '  release         Prepare for a GitHub release.'
	@echo '  clean           Delete generated files.'


install : $(TAR)
	$(MAKE) -C go install
	$(MAKE) -C lisp install


test :
	$(MAKE) -C go test
	$(MAKE) -C lisp test


clean :
	$(MAKE) -C go clean
	$(MAKE) -C lisp clean


release : assert-clean-worktree assert-on-master lisp-release git-release github-browse-release
	@echo 'Release successful.'


assert-on-master :
	@if [ "$$(git rev-parse --abbrev-ref HEAD)" != master ]; then \
		echo "Attempting to deploy non-master branch to production."; exit 1; \
	fi

assert-clean-worktree :
	@if ! git diff-index --quiet HEAD -- ; then \
		echo "Work tree is dirty. Aborting release."; exit 1; \
	fi

lisp-release :
	$(MAKE) -C lisp release

git-release :
	@while true; do \
		read -p "Push to GitHub? [Yn]: " CHOICE; \
		case $$CHOICE in \
			[Yy]|"") git push --quiet origin master "$${TAG}"; break;; \
			[Nn]   ) echo "Remember to push the tag with your changes."; exit;; \
		esac \
	done

github-browse-release :
	@export TAG="$$(git describe --abbrev=0 --tags)"; \
	python -mwebbrowser "https://$(REPO)/releases/tag/$$TAG"
