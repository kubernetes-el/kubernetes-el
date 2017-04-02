;;; context-compile-test.el --- Tests for compiled representation of contexts.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-and-compile
  (require 'f)

  (defvar project-root
    (locate-dominating-file default-directory ".git"))

  (defvar this-directory
    (f-join project-root "test")))

(require 'kubernetes (f-join project-root "kubernetes.el"))

(defconst sample-config-view-response
  (let* ((path (f-join this-directory "config-view-output.json"))
         (sample-response (f-read-text path)))
    (json-read-from-string sample-response)))

(defun draw-context-section (state)
  (kubernetes--eval-ast (kubernetes--render-context-section state)))


;; Shows "Fetching..." when state isn't initialized yet.

(defconst drawing-context-section-loading-result
  (s-trim-left "

Context:    Fetching...
"))

(ert-deftest drawing-context-section--empty-state ()
  (with-temp-buffer
    (save-excursion (magit-insert-section (root)
                      (draw-context-section nil)))
    (should (equal drawing-context-section-loading-result
                   (substring-no-properties (buffer-string))))
    (search-forward-regexp (rx "Context:" (+ space)))
    (should (equal 'magit-dimmed (get-text-property (point) 'face)))))


;; When there is a namespace set but no context, show the namespace with <none>
;; for the context label.

(defconst drawing-context-section-just-namespace
  (s-trim-left "

Context:    <none>
Namespace:  example-ns

"))

(ert-deftest drawing-context-section--empty-state ()
  (with-temp-buffer
    (save-excursion (magit-insert-section (root)
                      (draw-context-section '((current-namespace . "example-ns")))))
    (should (equal drawing-context-section-just-namespace
                   (substring-no-properties (buffer-string))))
    (search-forward-regexp (rx "Context:" (+ space)))
    (should (equal 'magit-dimmed (get-text-property (point) 'face)))))


;; When state is initialized, shows current information.

(defconst drawing-context-section-expected-result
  (s-trim-left "

Context:    example-prod
Cluster:    example-prod-cluster
Namespace:  example-ns

"))

(ert-deftest drawing-context-section--with-example-response ()
  (let ((input-state `((current-namespace . "example-ns")
                       (config . ,sample-config-view-response))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-context-section input-state)))
      (should (equal drawing-context-section-expected-result
                     (substring-no-properties (buffer-string)))))))

(ert-deftest drawing-context-section--context-line ()
  (let ((input-state `((current-namespace . "example-ns")
                       (config . ,sample-config-view-response))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-context-section input-state)))
      (search-forward "Context:")
      (should (equal "example-prod" (get-text-property (point) 'kubernetes-copy)))
      (skip-chars-forward " ")
      (should (equal 'kubernetes-context-name (get-text-property (point) 'face))))))

(ert-deftest drawing-context-section--namespace-line ()
  (let ((input-state `((current-namespace . "example-ns")
                       (config . ,sample-config-view-response))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-context-section input-state)))
      (search-forward "Namespace:")
      (should (equal "example-ns" (get-text-property (point) 'kubernetes-copy))))))

(ert-deftest drawing-context-section--cluster-line ()
  (let ((input-state `((current-namespace . "example-ns")
                       (config . ,sample-config-view-response))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-context-section input-state)))
      (search-forward "Cluster:")
      (should (equal "example-prod-cluster" (get-text-property (point) 'kubernetes-copy))))))


;;; context-compile-test.el ends here
