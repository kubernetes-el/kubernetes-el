;;; test-helper.el --- Setup run before tests.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Declare useful variables.

(require 'f)

(autoload 'json-read-from-string "json")

(eval-and-compile
  (defvar project-root
    (locate-dominating-file default-directory "Cask"))

  (defvar this-directory
    (f-join project-root "test")))

(add-to-list 'load-path project-root)


;; Initialize test coverage.

(when (require 'undercover nil t)
  (eval '(undercover "*.el" (:exclude "kubernetes-evil.el"))))


;; Load package

(require 'kubernetes (f-join project-root "kubernetes.el"))


;; Resources

(defun test-helper-string-resource (name)
  (let ((path (f-join this-directory "resources" name)))
    (f-read-text path)))

(defun test-helper-json-resource (name)
  (let* ((path (f-join this-directory "resources" name))
         (sample-response (f-read-text path)))
    (json-read-from-string sample-response)))


;; Helpers

(defmacro should-assert (form)
  `(let ((debug-on-error nil))
     (should-error ,form :type 'cl-assertion-failed)))

(defmacro test-helper-with-empty-state (&rest body)
  (declare (indent 0))
  `(let ((kubernetes-state--current-state nil)
         (kubernetes-kubectl-flags))
     ,@body))


(defvar test-helper-loaded t)

;;; test-helper.el ends here
