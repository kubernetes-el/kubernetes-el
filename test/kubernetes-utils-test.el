;;; kubernetes-utils-test.el --- Test util functions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(require 'kubernetes-overview)
(require 'kubernetes-utils)
(declare-function test-helper-json-resource "test-helper.el")

(ert-deftest kubernetes-utils-test--get-pod-container-names--invalid-input ()
  (should (equal nil (kubernetes-get-pod-container-names '()))))

(ert-deftest kubernetes-utils-test--get-pod-container-names ()
  (-let* ((res (test-helper-json-resource "get-pods-response.json"))
          ((&alist 'items [pod]) res))
    (should (equal '("example-service-1") (kubernetes-get-pod-container-names pod)))))

(ert-deftest kubernetes-utils-test--maybe-pod-name-at-point--not-in-overview-buffer ()
  (ignore-errors
    (kill-buffer kubernetes-overview-buffer-name))
  (should-not (kubernetes-utils-maybe-pod-name-at-point)))

(ert-deftest kubernetes-utils-test--read-container-name--no-state ()
  (cl-letf (((symbol-function 'kubernetes-state) #'ignore))
    (should-error (kubernetes-utils-read-container-name) :type 'kubernetes-state-error)))

;;; kubernetes-utils-test.el ends here
