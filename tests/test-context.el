;;; test-context.el --- Tests for context manipulation -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load-file "./tests/undercover-init.el")

(describe "Contexts"
  (describe "Renaming"
    (xit "Errors on non-existent context")
    (xit "Errors on name conflict")
    (xit "Renames in the kubectl config file")
    (xit "Defaults to the current context")
    (describe "When renaming the current context"
      (xit "Updates kubernetes-state"))))


;;; test-context.el ends here
