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


(describe "kubernetes--time-diff-string"
  (it "truncates to coarsest unit"
    (expect (kubernetes--time-diff-string 10 15) :to-equal "5s")
    (expect (kubernetes--time-diff-string 10 140) :to-equal "2m")
    (expect (kubernetes--time-diff-string 10 4000) :to-equal "1h")))

;;; test-core.el ends here
