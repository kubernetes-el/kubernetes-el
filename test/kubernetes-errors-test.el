;;; kubernetes-errors-test.el --- Test rendering of the errors section  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'f)
(require 'kubernetes-ast)
(require 'kubernetes-state)
(require 'kubernetes-errors)

(defun draw-errors-section (state)
  (kubernetes-ast-eval (kubernetes-errors-render state)))


;; Empty if there is no last error.

(ert-deftest kubernetes-errors-test--no-last-error ()
  (test-helper-with-empty-state
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-errors-section nil)))
      (should (string-blank-p (buffer-string))))))


;; Displays last-error if set

(defconst kubernetes-errors-test-expected-result
  "kubectl command failed

  Error message

Command:  bash

")

(ert-deftest kubernetes-errors-test--last-error-set ()
  (test-helper-with-empty-state
    (kubernetes-state-update-last-error "Error message" "bash" (current-time))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-errors-section (kubernetes-state))))
      (should (equal kubernetes-errors-test-expected-result
                     (substring-no-properties (buffer-string)))))))


;;; kubernetes-errors-test.el ends here
