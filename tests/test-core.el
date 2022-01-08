;;; test-core.el --- Tests for core functionality. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load-file "./tests/undercover-init.el")

(require 'kubernetes-core)

(describe "kubernetes--val-from-arg-list"
  (it "recognizes conjoined flag-value pairs"
    (expect
     (kubernetes--val-from-arg-list '("--foo=111") 'foo)
     :to-equal
     "111"))
  (it "recognizes separated flag-value pairs"
    (expect (kubernetes--val-from-arg-list '("--foo" "111") 'foo) :to-equal
            "111"))
  (it "returns nil on nil arg-list"
    (expect (kubernetes--val-from-arg-list nil 'foo) :to-equal nil))
  (it "returns nil on flag not found"
    (expect (kubernetes--val-from-arg-list '("--foo" "bar") 'baz) :to-equal nil)))

;;; test-core.el ends here
