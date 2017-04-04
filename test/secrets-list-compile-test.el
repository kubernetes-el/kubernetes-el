;;; secrets-list-compile-test.el --- Test rendering of the secrets list  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-and-compile
  (require 'f)

  (defvar project-root
    (locate-dominating-file default-directory ".git"))

  (defvar this-directory
    (f-join project-root "test")))

(require 'kubernetes (f-join project-root "kubernetes.el"))

(defconst sample-get-secrets-response
  (let* ((path (f-join this-directory "get-secrets-response.json"))
         (sample-response (f-read-text path)))
    (json-read-from-string sample-response)))

(defun draw-secrets-section (state)
  (kubernetes--eval-ast (kubernetes--render-secrets-section state)))


;; Shows "Fetching..." when state isn't initialized yet.

(defconst drawing-secrets-section-loading-result
  (s-trim-left "

Secrets
  Name                                            Data    Age
  Fetching...

"))

(ert-deftest drawing-secrets-section--empty-state ()
  (with-temp-buffer
    (save-excursion (magit-insert-section (root)
                      (draw-secrets-section nil)))
    (should (equal drawing-secrets-section-loading-result
                   (substring-no-properties (buffer-string))))
    (forward-line 1)
    (forward-to-indentation)
    (should (equal 'kubernetes-progress-indicator (get-text-property (point) 'face)))))


;; Shows "None" when there are no secrets.

(defconst drawing-secrets-section-empty-result
  (s-trim-left "

Secrets (0)
  None.

"))

(ert-deftest drawing-secrets-section--no-secrets ()
  (let ((empty-state `((secrets . ((items . ,(vector)))))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-secrets-section empty-state)))
      (should (equal drawing-secrets-section-empty-result
                     (substring-no-properties (buffer-string))))
      (search-forward "None")
      (should (equal 'magit-dimmed (get-text-property (point) 'face))))))


;; Shows secret lines when there are secrets.

(defconst drawing-secrets-section-sample-result
  (s-trim-left "

Secrets (2)
  Name                                            Data    Age
  example-secret-1                                   2    79d
    Namespace:  example-ns
    Created:    2017-01-13T00:24:47Z

  example-secret-2                                   1   331d
    Namespace:  example-ns
    Created:    2016-05-06T02:54:41Z


"))

(ert-deftest drawing-secrets-section--sample-response ()
  (let ((state `((secrets . ,sample-get-secrets-response)
                 (current-time . ,(date-to-time "2017-04-03 00:00Z")))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-secrets-section state)))
      (should (equal drawing-secrets-section-sample-result
                     (substring-no-properties (buffer-string)))))))

(ert-deftest drawing-secrets-section--sample-response-text-properties ()
  (let ((state `((secrets . ,sample-get-secrets-response))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-secrets-section state)))
      ;; Skip past header.
      (forward-line 2)

      (dolist (key '("Namespace"
                     "Created"))
        (save-excursion
          (search-forward key)
          (should (equal 'magit-header-line (get-text-property (point) 'face))))))))

;;; secrets-list-compile-test.el ends here
