;;; kubernetes-ingress-test.el --- Test rendering of the ingress list  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'kubernetes-ingress)
(declare-function test-helper-json-resource "test-helper.el")


(defconst sample-get-ingress-response (test-helper-json-resource "get-ingress-response.json"))

(defun draw-ingress-section (state)
  (kubernetes-ast-eval `(ingress-list ,state)))


;; Shows "Fetching..." when state isn't initialized yet.

(defconst kubernetes-ingress-test--loading-result
  (s-trim-left "

Ingress
  Name                                          Hosts                                  Address        Age
  Fetching...

"))

(ert-deftest kubernetes-ingress-test--empty-state ()
  (with-temp-buffer
    (save-excursion (magit-insert-section (root)
                      (draw-ingress-section nil)))
    (should (equal kubernetes-ingress-test--loading-result
                   (substring-no-properties (buffer-string))))
    (forward-line 1)
    (forward-to-indentation)
    (should (equal 'kubernetes-progress-indicator (get-text-property (point) 'face)))))


;; Shows "None" when there are no ingress.

(defconst kubernetes-ingress-test--empty-result
  (s-trim-left "

Ingress (0)
  None.

"))

(ert-deftest kubernetes-ingress-test--no-ingress ()
  (let ((empty-state `((ingress . ((items . ,(vector)))))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-ingress-section empty-state)))
      (should (equal kubernetes-ingress-test--empty-result
                     (substring-no-properties (buffer-string))))
      (search-forward "None")
      (should (equal 'magit-dimmed (get-text-property (point) 'face))))))


;; Shows ingress lines when there are ingress.

(defconst kubernetes-ingress-test--sample-result
  (s-trim-left "

Ingress (1)
  Name                                          Hosts                                  Address        Age
  kudos-ingress                                 myminikube.info                 192.168.99.100         1h
    Namespace:  default
    Created:    2018-07-10T11:43:41Z



"))

(ert-deftest kubernetes-ingress-test--sample-response ()
  (let ((state `((ingress . ,sample-get-ingress-response)
                 (current-time . ,(date-to-time "2017-04-03 00:00Z")))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-ingress-section state)))
      (should (equal kubernetes-ingress-test--sample-result
                     (substring-no-properties (buffer-string)))))))

(ert-deftest kubernetes-ingress-test--sample-response-text-properties ()
  (let ((state `((ingress . ,sample-get-ingress-response))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-ingress-section state)))
      ;; Skip past header.
      (forward-line 2)

      (dolist (key '("Namespace"
                     "Created"))
        (save-excursion
          (search-forward key)
          (should (equal 'magit-header-line (get-text-property (point) 'face))))))))


;;; kubernetes-ingress-test.el ends here
