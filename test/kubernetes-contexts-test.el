;;; kubernetes-contexts-test.el --- Tests for compiled representation of contexts.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'kubernetes-ast)
(require 'kubernetes-contexts)
(declare-function test-helper-json-resource "test-helper.el")

(defconst sample-config-view-response (test-helper-json-resource "config-view-response.json"))

(defun draw-context-section (state)
  (kubernetes-ast-eval (kubernetes-contexts-render state)))


;; Shows "Fetching..." when state isn't initialized yet.

(defconst kubernetes-contexts-test--loading-result
  (s-trim-left "

Context:    Fetching...
"))

(ert-deftest kubernetes-contexts-test--empty-state ()
  (with-temp-buffer
    (save-excursion (magit-insert-section (root)
                      (draw-context-section nil)))
    (should (equal kubernetes-contexts-test--loading-result
                   (substring-no-properties (buffer-string))))
    (search-forward-regexp (rx "Context:" (+ space)))
    (should (equal 'magit-dimmed (get-text-property (point) 'face)))))


;; When there is a namespace set but no context, show the namespace with <none>
;; for the context label.

(defconst kubernetes-contexts-test--just-namespace
  (s-trim-left "

Context:    <none>
Namespace:  example-ns

"))

(ert-deftest kubernetes-contexts-test--empty-state ()
  (with-temp-buffer
    (save-excursion (magit-insert-section (root)
                      (draw-context-section '((current-namespace . "example-ns")))))
    (should (equal kubernetes-contexts-test--just-namespace
                   (substring-no-properties (buffer-string))))
    (search-forward-regexp (rx "Context:" (+ space)))
    (should (equal 'magit-dimmed (get-text-property (point) 'face)))))


;; When state is initialized, shows current information.

(defconst kubernetes-contexts-test--expected-result
  (s-trim-left "

Context:    example-prod
Cluster:    example-prod-cluster
Namespace:  example-ns

"))

(ert-deftest kubernetes-contexts-test--with-example-response ()
  (let ((input-state `((current-namespace . "example-ns")
                       (config . ,sample-config-view-response))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-context-section input-state)))
      (should (equal kubernetes-contexts-test--expected-result
                     (substring-no-properties (buffer-string)))))))

(ert-deftest kubernetes-contexts-test--context-line ()
  (let ((input-state `((current-namespace . "example-ns")
                       (config . ,sample-config-view-response))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-context-section input-state)))
      (search-forward "Context:")
      (should (equal "example-prod" (get-text-property (point) 'kubernetes-copy)))
      (skip-chars-forward " ")
      (should (equal 'kubernetes-context-name (get-text-property (point) 'face))))))

(ert-deftest kubernetes-contexts-test--namespace-line ()
  (let ((input-state `((current-namespace . "example-ns")
                       (config . ,sample-config-view-response))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-context-section input-state)))
      (search-forward "Namespace:")
      (should (equal "example-ns" (get-text-property (point) 'kubernetes-copy))))))

(ert-deftest kubernetes-contexts-test--cluster-line ()
  (let ((input-state `((current-namespace . "example-ns")
                       (config . ,sample-config-view-response))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-context-section input-state)))
      (search-forward "Cluster:")
      (should (equal "example-prod-cluster" (get-text-property (point) 'kubernetes-copy))))))

;; When there is no namespace in the context, uses the default namespace.

(defconst sample-config-view-response-no-namespace (test-helper-json-resource "config-view-response-no-namespace.json"))


(defconst kubernetes-contexts-test--expected-result-when-no-namespace
  (s-trim-left "

Context:    example-prod
Cluster:    example-prod-cluster
Namespace:  default

"))

(ert-deftest kubernetes-contexts-test--with-example-response--no-namespace ()
  (let ((input-state `((current-namespace . nil)
                       (config . ,sample-config-view-response-no-namespace))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-context-section input-state)))
      (should (equal kubernetes-contexts-test--expected-result-when-no-namespace
                     (substring-no-properties (buffer-string)))))))

;;; kubernetes-contexts-test.el ends here
