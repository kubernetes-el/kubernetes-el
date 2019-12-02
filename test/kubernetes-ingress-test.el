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

Ingress (4)
  Name                                          Hosts                                  Address        Age
  clojurescript-ingress                         domain.example.io         3.10.144.54, 3.10.151.11, 3.9.177.209        -2y
    Namespace:  default
    Created:    2019-11-13T14:51:00Z

  example-ingress                               myminikube.info                 192.168.99.100        29d
    Namespace:  default
    Created:    2018-06-10T11:43:41Z

                                                myminikube.info                 192.168.99.100        29d
    Namespace:  default
    Created:    2018-06-10T11:43:41Z

  example-ingress                               myminikube.info           192.168.99.100, 1.1.1.1, 2.2.2.2        29d
    Namespace:  default
    Created:    2018-06-10T11:43:41Z


"))

(ert-deftest kubernetes-ingress-test--sample-response ()
  (let ((state `((ingress . ,sample-get-ingress-response)
                 (current-time . ,(date-to-time "2018-07-10 10:43Z")))))
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
