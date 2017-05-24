.PHONY: build install help test clean release


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


release :
	$(MAKE) -C lisp release
