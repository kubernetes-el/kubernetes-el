CASK = cask

.PHONY: check


check : .cask
	$(CASK) exec ert-runner


.cask :
	$(CASK) install
