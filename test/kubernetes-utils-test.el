;;; kubernetes-utils-test.el --- Test util functions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'kubernetes-overview)
(require 'kubernetes-utils)

(ert-deftest kubernetes-utils-test--maybe-pod-name-at-point--not-in-overview-buffer ()
  (ignore-errors
    (kill-buffer kubernetes-overview-buffer-name))
  (should-not (kubernetes-utils-maybe-pod-name-at-point)))

(ert-deftest kubernetes-utils-test--read-container-name--no-state ()
  (cl-letf (((symbol-function 'kubernetes-state) #'ignore))
    (should-error (kubernetes-utils-read-container-name) :type 'k8s-state-error)))

;;; kubernetes-utils-test.el ends here
