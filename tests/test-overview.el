;;; test-overview.el --- Tests for kubernetes-overview.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load-file "./tests/undercover-init.el")

(require 'kubernetes-overview)

(describe "kubernetes-overview"
  (describe "when kubectl not found"
    (before-each
      (spy-on 'executable-find :and-return-value nil))
    (it "fails"
      (expect (kubernetes-overview) :to-throw 'error))))

;;; test-overview.el ends here
