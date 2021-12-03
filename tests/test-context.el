;;; test-context.el --- Tests for context manipulation -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load-file "./tests/undercover-init.el")
(require 'kubernetes-contexts)
(require 'kubernetes-state)
(require 'kubernetes-props)

(describe "Context"
  (describe "renaming"
    (before-each
     (spy-on
      'kubernetes-contexts--context-names
      :and-return-value
      '("context0" "context1" "context2"))
     (spy-on 'kubernetes-state)
     (spy-on 'kubernetes-kubectl))

    (it "errors on non-existent context"
      (expect (kubernetes-contexts-rename "nonexistent" "new-name") :to-throw 'error))

    (it "errors on name conflict"
      (expect (kubernetes-contexts-rename "context0" "context1") :to-throw 'error))))


;;; test-context.el ends here
