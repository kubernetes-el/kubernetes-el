;;; kubernetes-secrets-test.el --- Test rendering of the secrets list  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'f)
(require 'kubernetes-secrets)
(declare-function test-helper-json-resource "test-helper.el")


(defconst sample-get-secrets-response (test-helper-json-resource "get-secrets-response.json"))

(defun draw-secrets-section (state)
  (kubernetes-ast-eval `(secrets-list ,state)))


;; Shows "Fetching..." when state isn't initialized yet.

(defconst kubernetes-secrets-test--loading-result
  (s-trim-left "

Secrets
  Name                                            Data    Age
  Fetching...

"))

(ert-deftest kubernetes-secrets-test---empty-state ()
  (with-temp-buffer
    (save-excursion (magit-insert-section (root)
                      (draw-secrets-section nil)))
    (should (equal kubernetes-secrets-test--loading-result
                   (substring-no-properties (buffer-string))))
    (forward-line 1)
    (forward-to-indentation)
    (should (equal 'kubernetes-progress-indicator (get-text-property (point) 'face)))))


;; Shows "None" when there are no secrets.

(defconst kubernetes-secrets-test--empty-result
  (s-trim-left "

Secrets (0)
  None.

"))

(ert-deftest kubernetes-secrets-test---no-secrets ()
  (let ((empty-state `((secrets . ((items . ,(vector)))))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-secrets-section empty-state)))
      (should (equal kubernetes-secrets-test--empty-result
                     (substring-no-properties (buffer-string))))
      (search-forward "None")
      (should (equal 'magit-dimmed (get-text-property (point) 'face))))))


;; Shows secret lines when there are secrets.

(defconst kubernetes-secrets-test--sample-result
  (s-trim-left "

Secrets (2)
  Name                                            Data    Age
  example-secret-1                                   2    79d
    Namespace:  example-ns
    Created:    2017-01-13T00:24:47Z

  example-secret-2                                   1   331d
    Namespace:  example-ns
    Created:    2016-05-06T02:54:41Z


"))

(ert-deftest kubernetes-secrets-test---sample-response ()
  (let ((state `((secrets . ,sample-get-secrets-response)
                 (current-time . ,(date-to-time "2017-04-03 00:00Z")))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-secrets-section state)))
      (should (equal kubernetes-secrets-test--sample-result
                     (substring-no-properties (buffer-string)))))))

(ert-deftest kubernetes-secrets-test---sample-response-text-properties ()
  (let ((state `((secrets . ,sample-get-secrets-response)
                 (current-time . ,(date-to-time "2017-04-03 00:00Z")))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-secrets-section state)))
      ;; Skip past header.
      (forward-line 2)

      (dolist (key '("Namespace"
                     "Created"))
        (save-excursion
          (search-forward key)
          (should (equal 'magit-header-line (get-text-property (point) 'face))))))))


;;; kubernetes-secrets-test.el ends here
