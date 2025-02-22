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
      (should (equal 'kubernetes-dimmed (get-text-property (point) 'face))))))


;; Shows ingress lines when there are ingress.

(defconst kubernetes-ingress-test--sample-result
  (s-trim-left "

Ingress (4)
  Name                                          Hosts                                  Address        Age
  clojurescript-ingress                         domain.example.io           3.10.144.54, 3....        -2y
    Namespace:  default
    Created:    2019-11-13T14:51:00Z

  example-ingress                               myminikube.info                 192.168.99.100        29d
    Namespace:  default
    Created:    2018-06-10T11:43:41Z

                                                myminikube.info                 192.168.99.100        29d
    Namespace:  default
    Created:    2018-06-10T11:43:41Z

  example-ingress                               myminikube.info, myl...     192.168.99.100,...        29d
    Namespace:  default
    Created:    2018-06-10T11:43:41Z


"))

(ert-deftest kubernetes-ingress-test--sample-response ()
  (let ((state `((ingress . ,sample-get-ingress-response)
                 (current-time . ,(parse-iso8601-time-string "2018-07-10T10:43:00Z")))))
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
          (should (equal 'magit-section-heading (get-text-property (point) 'face))))))))

(ert-deftest kubernetes-ingress-time-debug ()
  (let* ((creation "2019-11-13T14:51:00Z")
         (ref-time "2018-07-10T10:43:00Z")
         (start (parse-iso8601-time-string creation))
         (now (parse-iso8601-time-string ref-time)))
    (message "Debug age calculation:
Creation: %s -> %S
Reference: %s -> %S
Difference in years: %s"
            creation start
            ref-time now
            (kubernetes--time-diff-string start now))))

(ert-deftest kubernetes-time-parse-comparison ()
  (let* ((creation "2019-11-13T14:51:00Z")
         (ref-time "2018-07-10T10:43:00Z")
         (dt-creation (date-to-time creation))
         (dt-ref (date-to-time ref-time))
         (iso-creation (parse-iso8601-time-string creation))
         (iso-ref (parse-iso8601-time-string ref-time)))
    (message "Time parsing debug:
Creation time (%s):
  date-to-time: %S
  parse-iso8601: %S
Reference time (%s):
  date-to-time: %S
  parse-iso8601: %S

Time differences:
  date-to-time diff: %S
  parse-iso8601 diff: %S

Float differences:
  date-to-time days: %f
  parse-iso8601 days: %f"
            creation
            dt-creation
            iso-creation
            ref-time
            dt-ref
            iso-ref
            (time-subtract dt-creation dt-ref)
            (time-subtract iso-creation iso-ref)
            (/ (float-time (time-subtract dt-creation dt-ref)) 86400.0)
            (/ (float-time (time-subtract iso-creation iso-ref)) 86400.0))))
;;; kubernetes-ingress-test.el ends here
