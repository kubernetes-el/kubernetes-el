;;; kubernetes-configmaps-test.el --- Test rendering of the configmaps list  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'kubernetes-configmaps)
(declare-function test-helper-json-resource "test-helper.el")


(defconst sample-get-configmaps-response (test-helper-json-resource "get-configmaps-response.json"))

(defun draw-configmaps-section (state)
  (kubernetes-ast-eval `(configmaps-list ,state)))


;; Shows "Fetching..." when state isn't initialized yet.

(defconst kubernetes-configmaps-test--loading-result
  (s-trim-left "

Configmaps
  Name                                            Data    Age
  Fetching...

"))

(ert-deftest kubernetes-configmaps-test--empty-state ()
  (with-temp-buffer
    (save-excursion (magit-insert-section (root)
                      (draw-configmaps-section nil)))
    (should (equal kubernetes-configmaps-test--loading-result
                   (substring-no-properties (buffer-string))))
    (forward-line 1)
    (forward-to-indentation)
    (should (equal 'kubernetes-progress-indicator (get-text-property (point) 'face)))))


;; Shows "None" when there are no configmaps.

(defconst kubernetes-configmaps-test--empty-result
  (s-trim-left "

Configmaps (0)
  None.

"))

(ert-deftest kubernetes-configmaps-test--no-configmaps ()
  (let ((empty-state `((configmaps . ((items . ,(vector)))))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-configmaps-section empty-state)))
      (should (equal kubernetes-configmaps-test--empty-result
                     (substring-no-properties (buffer-string))))
      (search-forward "None")
      (should (equal 'magit-dimmed (get-text-property (point) 'face))))))


;; Shows configmap lines when there are configmaps.

(defconst kubernetes-configmaps-test--sample-result
  (s-trim-left "

Configmaps (2)
  Name                                            Data    Age
  example-configmap-1                                2    79d
    Namespace:  example-ns
    Created:    2017-01-13T00:24:47Z

  example-configmap-2                                1   331d
    Namespace:  example-ns
    Created:    2016-05-06T02:54:41Z


"))

(ert-deftest kubernetes-configmaps-test--sample-response ()
  (let ((state `((configmaps . ,sample-get-configmaps-response)
                 (current-time . ,(date-to-time "2017-04-03 00:00Z")))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-configmaps-section state)))
      (should (equal kubernetes-configmaps-test--sample-result
                     (substring-no-properties (buffer-string)))))))

(ert-deftest kubernetes-configmaps-test--sample-response-text-properties ()
  (let ((state `((configmaps . ,sample-get-configmaps-response))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-configmaps-section state)))
      ;; Skip past header.
      (forward-line 2)

      (dolist (key '("Namespace"
                     "Created"))
        (save-excursion
          (search-forward key)
          (should (equal 'magit-header-line (get-text-property (point) 'face))))))))


;;; kubernetes-configmaps-test.el ends here
