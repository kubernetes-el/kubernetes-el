;;; kubernetes-pods-test.el --- Test rendering of the pods list  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'kubernetes-pods)
(declare-function test-helper-json-resource "test-helper.el")


(defconst sample-get-pods-response (test-helper-json-resource "get-pods-response.json"))

(defun draw-pods-section (state)
  (kubernetes-ast-eval `(pods-list ,state)))


;; Shows "Fetching..." when state isn't initialized yet.

(defconst kubernetes-pods-test--loading-result
  (s-trim-left "

Pods
  Name                                          Status     Ready   Restarts    Age
  Fetching...

"))

(ert-deftest kubernetes-pods-test--empty-state ()
  (with-temp-buffer
    (save-excursion (magit-insert-section (root)
                      (draw-pods-section nil)))
    (should (equal kubernetes-pods-test--loading-result
                   (substring-no-properties (buffer-string))))
    (forward-line 1)
    (forward-to-indentation)
    (should (equal 'kubernetes-progress-indicator (get-text-property (point) 'face)))))


;; Shows "None" when there are no pods.

(defconst kubernetes-pods-test--empty-result
  (s-trim-left "

Pods (0)
  None.

"))

(ert-deftest kubernetes-pods-test--no-pods ()
  (let ((empty-state `((pods . ((items . ,(vector)))))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-pods-section empty-state)))
      (should (equal kubernetes-pods-test--empty-result
                     (substring-no-properties (buffer-string))))
      (search-forward "None")
      (should (equal 'magit-dimmed (get-text-property (point) 'face))))))


;; Shows pod lines when there are pods.

(defconst kubernetes-pods-test--sample-result
  (s-trim-left "

Pods (3)
  Name                                          Status     Ready   Restarts    Age
  example-svc-v3-1603416598-2f9lb               Running      1/1          0    36d
    Name:       example-service-2
    Label:      example-pod-v3
    Namespace:  ns.example
    Image:      example.com/example-service:3.0.0
    Host IP:    10.0.0.0
    Pod IP:     172.0.0.1
    Started:    2017-02-25T08:12:14Z

  example-svc-v4-1603416598-2f9lb               Running      1/1          0    36d
    Name:       example-service-4
    Label:      example-pod-v4
    Namespace:  ns.example
    Image:      example.com/example-service:4.8.0
    Host IP:    10.0.0.0
    Pod IP:     172.0.0.1
    Started:    2017-02-25T08:12:14Z

  example-svc-v5-1603416598-2f9lb               Running      0/0          0    36d
    Name:       N/A
    Label:      example-pod-v5
    Namespace:  ns.example
    Image:      N/A
    Host IP:    10.0.0.0
    Pod IP:     172.0.0.1
    Started:    2017-02-25T08:12:14Z


"))

(ert-deftest kubernetes-pods-test--sample-response ()
  (let ((state `((pods . ,sample-get-pods-response)
                 (current-time . ,(date-to-time "2017-04-03 00:00Z")))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-pods-section state)))
      (should (equal kubernetes-pods-test--sample-result
                     (substring-no-properties (buffer-string)))))))


(ert-deftest kubernetes-pods-test--sample-response-text-properties ()
  (let ((state `((pods . ,sample-get-pods-response)
                 (current-time . ,(date-to-time "2017-04-03 00:00Z")))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-pods-section state)))
      ;; Skip past header.
      (forward-line 2)

      (dolist (key '("Name"
                     "Label"
                     "Namespace"
                     "Image"
                     "Host IP"
                     "Pod IP"
                     "Started"))
        (save-excursion
          (search-forward key)
          (should (equal 'magit-header-line (get-text-property (point) 'face))))))))

;;; kubernetes-pods-test.el ends here
