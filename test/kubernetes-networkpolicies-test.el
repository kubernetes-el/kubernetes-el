;;; kubernetes-networkpolicies-test.el --- Test rendering of the network policies list  -*- lexical-binding: t; show-trailing-whitespace: t; -*-
;;; Commentary:
;;; Code:

(require 'ert)
(require 'kubernetes-networkpolicies)
(require 's)

(declare-function test-helper-json-resource "test-helper.el")

(defconst sample-get-networkpolicies-response (test-helper-json-resource "get-networkpolicies-response.json"))

(defun draw-networkpolicies-section (state)
  (kubernetes-ast-eval `(networkpolicies-list ,state)))

;; Shows "Fetching..." when state isn't initialized yet.

(defconst kubernetes-networkpolicies-test--loading-result
  (s-trim-left "Network Policies
  Name                                 Namespace       Ingress Egress
  Fetching...

"))

(ert-deftest kubernetes-networkpolicies-test--empty-state ()
  (with-temp-buffer
    (save-excursion (magit-insert-section (root)
                      (draw-networkpolicies-section nil)))
    (should (equal kubernetes-networkpolicies-test--loading-result
                   (substring-no-properties (buffer-string))))
    (forward-line 1)
    (forward-to-indentation)
    (should (equal 'kubernetes-progress-indicator (get-text-property (point) 'face)))))

;; Shows "None" when there are no network policies.

(defconst kubernetes-networkpolicies-test--empty-result
  (s-trim-left "

Network Policies (0)
  None.

"))

(ert-deftest kubernetes-networkpolicies-test--no-networkpolicies ()
  (let ((empty-state `((networkpolicies . ((items . ,(vector)))))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-networkpolicies-section empty-state)))
      (should (equal kubernetes-networkpolicies-test--empty-result
                     (substring-no-properties (buffer-string))))
      (search-forward "None")
      (should (equal 'kubernetes-dimmed (get-text-property (point) 'face))))))

;; Shows network policy lines when there are network policies.

(defconst kubernetes-networkpolicies-test--sample-result
  (s-trim-left "

Network Policies (2)
  Name                                 Namespace       Ingress Egress
  kube-prometheus-stack-grafana        monitoring          yes     no
    Namespace:  monitoring
    Name:       kube-prometheus-stack-grafana
    Created:    2025-02-13T07:40:18Z

  kube-prometheus-stack-prometheus     monitoring          yes    yes
    Namespace:  monitoring
    Name:       kube-prometheus-stack-prometheus
    Created:    2025-02-13T07:40:18Z


"))

(ert-deftest kubernetes-networkpolicies-test--sample-response ()
  (let ((state `((networkpolicies . ,sample-get-networkpolicies-response))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-networkpolicies-section state)))
      (should (equal kubernetes-networkpolicies-test--sample-result
                     (substring-no-properties (buffer-string)))))))

;;; kubernetes-networkpolicies-test.el ends here
