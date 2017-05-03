;;; kubernetes-deployments-test.el --- Test rendering of the deployments list  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'kubernetes-deployments)
(declare-function test-helper-json-resource "test-helper.el")


(defconst sample-get-deployments-response (test-helper-json-resource "get-deployments-response.json"))

(defun draw-deployments-section (state)
  (kubernetes-ast-eval `(deployments-list ,state)))


;; Shows "Fetching..." when state isn't initialized yet.

(defconst kubernetes-deployments-test--loading-result
  (s-trim-left "

Deployments
  Name                                            Replicas   UpToDate  Available    Age
  Fetching...

"))

(ert-deftest kubernetes-deployments-test--empty-state ()
  (with-temp-buffer
    (save-excursion (magit-insert-section (root)
                      (draw-deployments-section nil)))
    (should (equal kubernetes-deployments-test--loading-result
                   (substring-no-properties (buffer-string))))
    (forward-line 1)
    (forward-to-indentation)
    (should (equal 'kubernetes-progress-indicator (get-text-property (point) 'face)))))


;; Shows "None" when there are no deployments.

(defconst kubernetes-deployments-test--empty-result
  (s-trim-left "

Deployments (0)
  None.

"))

(ert-deftest kubernetes-deployments-test--no-deployments ()
  (let ((empty-state `((deployments . ((items . ,(vector)))))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-deployments-section empty-state)))
      (should (equal kubernetes-deployments-test--empty-result
                     (substring-no-properties (buffer-string))))
      (search-forward "None")
      (should (equal 'magit-dimmed (get-text-property (point) 'face))))))


;; Shows deployment lines when there are deployments.

(defconst kubernetes-deployments-test--sample-result
  (s-trim-left "

Deployments (2)
  Name                                            Replicas   UpToDate  Available    Age
  deployment-1                                         1/1          1          1    44d
    Selector:   example-pod-v3
    Namespace:  example-ns
    Created:    2017-02-17T02:23:30Z

  deployment-2                                         1/1          1          1    39d
    Selector:   deployment-2
    Namespace:  example-ns
    Created:    2017-02-22T02:15:36Z


"))

(ert-deftest kubernetes-deployments-test--sample-response ()
  (let ((state `((deployments . ,sample-get-deployments-response)
                 (current-time . ,(date-to-time "2017-04-03 00:00Z")))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-deployments-section state)))
      (should (equal kubernetes-deployments-test--sample-result
                     (substring-no-properties (buffer-string)))))))

(ert-deftest kubernetes-deployments-test--sample-response-text-properties ()
  (let ((state `((deployments . ,sample-get-deployments-response))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-deployments-section state)))
      ;; Skip past header.
      (forward-line 2)

      (dolist (key '("Namespace"
                     "Created"))
        (save-excursion
          (search-forward key)
          (should (equal 'magit-header-line (get-text-property (point) 'face))))))))


;;; kubernetes-deployments-test.el ends here
