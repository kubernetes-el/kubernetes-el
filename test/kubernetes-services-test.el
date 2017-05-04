;;; kubernetes-services-test.el --- Test rendering of the services list  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'kubernetes-services)
(declare-function test-helper-json-resource "test-helper.el")


(defconst sample-get-services-response (test-helper-json-resource "get-services-response.json"))

(defun draw-services-section (state)
  (kubernetes-ast-eval `(services-list ,state)))


;; Shows "Fetching..." when state isn't initialized yet.

(defconst kubernetes-services-test--loading-result
  (s-trim-left "

Services
  Name                               Internal IP     External IP    Age
  Fetching...

"))

(ert-deftest kubernetes-services-test--empty-state ()
  (with-temp-buffer
    (save-excursion (magit-insert-section (root)
                      (draw-services-section nil)))
    (should (equal kubernetes-services-test--loading-result
                   (substring-no-properties (buffer-string))))
    (forward-line 1)
    (forward-to-indentation)
    (should (equal 'kubernetes-progress-indicator (get-text-property (point) 'face)))))


;; Shows "None" when there are no services.

(defconst kubernetes-services-test--empty-result
  (s-trim-left "

Services (0)
  None.

"))

(ert-deftest kubernetes-services-test--no-services ()
  (let ((empty-state `((services . ((items . ,(vector)))))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-services-section empty-state)))
      (should (equal kubernetes-services-test--empty-result
                     (substring-no-properties (buffer-string))))
      (search-forward "None")
      (should (equal 'magit-dimmed (get-text-property (point) 'face))))))


;; Shows service lines when there are services.

(defconst kubernetes-services-test--sample-result
  (s-trim-left "

Services (2)
  Name                               Internal IP     External IP    Age
  example-svc-1                      192.168.0.0     192.168.0.0     4d
    Selector:      example-pod-v1
    Label:         example-svc-1
    Namespace:     example
    Created:       2017-03-29T21:42:56Z
    Internal IP:   192.168.0.0
    External IPs:  192.168.0.0
    Ports:         80/TCP

  example-svc-2                      192.168.0.0                    19d
    Selector:      example-pod-v1
    Label:         example-svc-2
    Namespace:     example
    Created:       2017-03-14T21:42:56Z
    Internal IP:   192.168.0.0
    Ports:         80/TCP


"))

(ert-deftest kubernetes-services-test--sample-response ()
  (let ((state `((services . ,sample-get-services-response)
                 (current-time . ,(date-to-time "2017-04-03 00:00Z")))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-services-section state)))
      (should (equal kubernetes-services-test--sample-result
                     (substring-no-properties (buffer-string)))))))

(ert-deftest kubernetes-services-test--sample-response-text-properties ()
  (let ((state `((services . ,sample-get-services-response)
                 (current-time . ,(date-to-time "2017-04-03 00:00Z")))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-services-section state)))
      ;; Skip past header.
      (forward-line 2)

      (dolist (key '("Namespace"
                     "Created"))
        (save-excursion
          (search-forward key)
          (should (equal 'magit-header-line (get-text-property (point) 'face))))))))


;;; kubernetes-services-test.el ends here
