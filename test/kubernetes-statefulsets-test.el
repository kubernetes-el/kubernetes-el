;;; kubernetes-statefulsets-test.el --- Test rendering of the statefulsets list  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'kubernetes-statefulsets)
(declare-function test-helper-json-resource "test-helper.el")


(defconst sample-get-statefulsets-response (test-helper-json-resource "get-statefulsets-response.json"))

(defun draw-statefulsets-section (state)
  (kubernetes-ast-eval `(statefulsets-list ,state)))


;; Shows "Fetching..." when state isn't initialized yet.

(defconst kubernetes-statefulsets-test--loading-result
  (s-trim-left "

Statefulsets
  Name                                            Replicas                          Age
  Fetching...

"))

(ert-deftest kubernetes-statefulsets-test--empty-state ()
  (with-temp-buffer
    (save-excursion (magit-insert-section (root)
                      (draw-statefulsets-section nil)))
    (should (equal kubernetes-statefulsets-test--loading-result
                   (substring-no-properties (buffer-string))))
    (forward-line 1)
    (forward-to-indentation)
    (should (equal 'kubernetes-progress-indicator (get-text-property (point) 'face)))))


;; Shows "None" when there are no statefulsets.

(defconst kubernetes-statefulsets-test--empty-result
  (s-trim-left "

Statefulsets (0)
  None.

"))

(ert-deftest kubernetes-statefulsets-test--no-statefulsets ()
  (let ((empty-state `((statefulsets . ((items . ,(vector)))))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-statefulsets-section empty-state)))
      (should (equal kubernetes-statefulsets-test--empty-result
                     (substring-no-properties (buffer-string))))
      (search-forward "None")
      (should (equal 'magit-dimmed (get-text-property (point) 'face))))))


;; Shows statefulset lines when there are statefulsets.

(defconst kubernetes-statefulsets-test--sample-result
  (s-trim-left "

Statefulsets (1)
  Name                                            Replicas                          Age
  postgres-pod                                         1/1                           1d
    Namespace:  example-ns
    Created:    2017-04-01T09:48:14Z


"))

(ert-deftest kubernetes-statefulsets-test--sample-response ()
  (let ((state `((statefulsets . ,sample-get-statefulsets-response)
                 (current-time . ,(date-to-time "2017-04-03 00:00Z")))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-statefulsets-section state)))
      (should (equal kubernetes-statefulsets-test--sample-result
                     (substring-no-properties (buffer-string)))))))

(ert-deftest kubernetes-statefulsets-test--sample-response-text-properties ()
  (let ((state `((statefulsets . ,sample-get-statefulsets-response))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-statefulsets-section state)))
      ;; Skip past header.
      (forward-line 2)

      (dolist (key '("Namespace"
                     "Created"))
        (save-excursion
          (search-forward key)
          (should (equal 'magit-header-line (get-text-property (point) 'face))))))))


;;; kubernetes-statefulsets-test.el ends here
