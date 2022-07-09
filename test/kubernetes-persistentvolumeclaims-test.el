;;; kubernetes-persistentvolumeclaims-test.el --- Test rendering of the persistentvolumeclaims list  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'kubernetes-persistentvolumeclaims)
(declare-function test-helper-json-resource "test-helper.el")


(defconst sample-get-persistentvolumeclaims-response (test-helper-json-resource "get-persistentvolumeclaims-response.json"))

(defun draw-persistentvolumeclaims-section (state)
  (kubernetes-ast-eval `(persistentvolumeclaims-list ,state)))


;; Shows "Fetching..." when state isn't initialized yet.

(defconst kubernetes-persistentvolumeclaims-test--loading-result
  (s-trim-left "

Persistent Volume Claims
  Name                          Phase   Capacity           Class    Age
  Fetching...

"))

(ert-deftest kubernetes-persistentvolumeclaims-test--initial-state ()
  (let ((state '()))
    (setf (alist-get 'persistentvolumeclaims-columns state) kubernetes-persistentvolumeclaims--default-columns)
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-persistentvolumeclaims-section state)))
      (should (equal kubernetes-persistentvolumeclaims-test--loading-result
                     (substring-no-properties (buffer-string))))
      (forward-line 1)
      (forward-to-indentation)
      (should (equal 'kubernetes-progress-indicator (get-text-property (point) 'face))))))


;; Shows "None" when there are no persistentvolumeclaims.

(defconst kubernetes-persistentvolumeclaims-test--empty-result
  (s-trim-left "

Persistent Volume Claims (0)
  None.

"))

(ert-deftest kubernetes-persistentvolumeclaims-test--no-persistentvolumeclaims ()
  (let ((empty-state `((persistentvolumeclaims . ((items . ,(vector)))))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-persistentvolumeclaims-section empty-state)))
      (should (equal kubernetes-persistentvolumeclaims-test--empty-result
                     (substring-no-properties (buffer-string))))
      (search-forward "None")
      (should (equal 'kubernetes-dimmed (get-text-property (point) 'face))))))


;; Shows persistent volume claim lines when there are persistentvolumeclaims.

(defconst kubernetes-persistentvolumeclaims-test--sample-result
  (s-trim-left "

Persistent Volume Claims (2)
  Name                          Phase   Capacity           Class    Age
  bar-pvc                       Bound        2Gi          manual     3h
    Namespace:  example-ns
    Created:    2021-11-05T20:53:33Z

  foo-pvc                     Pending        nil                     1d
    Namespace:  example-ns
    Created:    2021-11-04T20:41:52Z


"))

(ert-deftest kubernetes-persistentvolumeclaims-test--sample-response ()
  (let ((state `((persistentvolumeclaims-columns . ((Name (width -24))
                                   (Phase (width 10))
                                   (Capacity (width 10))
                                   (Class (width 15))
                                   (Age (width 6))))
                 (persistentvolumeclaims . ,sample-get-persistentvolumeclaims-response)
                 (current-time . ,(date-to-time "2021-11-06 00:00Z")))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-persistentvolumeclaims-section state)))
      (should (equal kubernetes-persistentvolumeclaims-test--sample-result
                     (substring-no-properties (buffer-string)))))))

(ert-deftest kubernetes-persistentvolumeclaims-test--sample-response-text-properties ()
  (let ((state `((persistentvolumeclaims . ,sample-get-persistentvolumeclaims-response))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-persistentvolumeclaims-section state)))
      ;; Skip past header.
      (forward-line 2)

      (dolist (key '("Namespace"
                     "Created"))
        (save-excursion
          (search-forward key)
          (should (equal 'magit-section-heading (get-text-property (point) 'face))))))))


;;; kubernetes-persistentvolumeclaims-test.el ends here
