;;; kubernetes-labels-test.el --- Tests for kubernetes-labels.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'kubernetes-ast)
(require 'kubernetes-labels)
(declare-function test-helper-json-resource "test-helper.el")

(defconst sample-get-pods-response (test-helper-json-resource "get-pods-response.json"))


;; Render "Fetching..." if there are no pods in the state.

(ert-deftest kubernetes-labels-test--no-pods-in-state ()
  (test-helper-with-empty-state
    (kubernetes-state-update-label-query "test")

    (with-temp-buffer
      (kubernetes-ast-eval `(labelled-pods-list ,(kubernetes-state)))
      (let ((result (substring-no-properties (buffer-string))))
        (should (string-match-p "Fetching..." result))))))

;; Render just the matches for the label query.

(ert-deftest kubernetes-labels-test--pods-match ()
  (test-helper-with-empty-state
    (kubernetes-state-update-label-query "example-pod-v3")
    (kubernetes-state-update-pods sample-get-pods-response)

    (with-temp-buffer
      (kubernetes-ast-eval `(labelled-pods-list ,(kubernetes-state)))
      (let ((result (substring-no-properties (buffer-string))))
        (should (string-match-p "example-pod-v3" result))
        (should-not (string-match-p "example-pod-v4" result))))))


(provide 'kubernetes-labels-test)

;;; kubernetes-labels-test.el ends here
