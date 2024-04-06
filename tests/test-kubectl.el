;;; test-kubectl.el --- Tests for kubectl. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load-file "./tests/undercover-init.el")

(require 'buttercup)
(require 'kubernetes-kubectl)

(describe "Kubectl interface"
  (describe "error handling"
    (it "writes message when overview buffer not selected"
      (spy-on 'message)
      (kubernetes-kubectl--default-error-handler "")
      (expect 'message :to-have-been-called))))
