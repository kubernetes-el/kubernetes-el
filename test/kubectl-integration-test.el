;;; kubectl-integration-test.el --- Tests for integration with kubectl  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-and-compile
  (require 'f)

  (defvar project-root
    (locate-dominating-file default-directory ".git"))

  (defvar this-directory
    (f-join project-root "test")))

(require 's)
(require 'json)
(require 'noflet)
(require 'kubernetes (f-join project-root "kubernetes.el"))

;; Test helpers

(defmacro with-successful-response-at (expected-args response-string form)
  "Rebind `kubernetes--kubectl' to test handling of successful responses.

Asserts the the args parsed to that function are equal to EXPECTED-ARGS.

Executes the on-success callback with a buffer containing RESPONSE-STRING.

FORM is the Elisp form to be evaluated, in which `kubernetes--kubectl'
will be mocked."
  (declare (indent 2))
  `(noflet ((kubernetes--kubectl
             (args on-success &optional on-error cleanup-cb)
             (let ((buf (generate-new-buffer " test")))
               (with-current-buffer buf
                 (unwind-protect
                     (progn
                       (insert ,response-string)
                       (should (equal args ,expected-args))
                       (funcall on-success buf)))
                 (when cleanup-cb
                   (funcall cleanup-cb))))))
     ,form))

(defmacro with-error-response-at (expected-args response-string form)
  "Rebind `kubernetes--kubectl' to test handling of failed responses.

Asserts the the args parsed to that function are equal to EXPECTED-ARGS.

Executes the on-error callback with a buffer containing RESPONSE-STRING.

FORM is the Elisp form to be evaluated, in which `kubernetes--kubectl'
will be mocked."
  (declare (indent 2))
  `(noflet ((kubernetes--kubectl
             (args _on-success &optional on-error cleanup-cb)
             (let ((buf (generate-new-buffer " test")))
               (with-current-buffer buf
                 (unwind-protect
                     (progn
                       (insert ,response-string)
                       (should (equal args ,expected-args))
                       (should on-error)
                       (funcall on-error buf))
                   (when cleanup-cb
                     (funcall cleanup-cb)))))))
     ,form))

;; Test cases

(ert-deftest running-kubectl-works ()
  (if (executable-find kubernetes-kubectl-executable)
      (let ((result-buffer (kubernetes--await-on-async
                            (lambda (cb)
                              (kubernetes--kubectl '("version" "--client") cb)))))
        (with-current-buffer result-buffer
          (should (string-prefix-p "Client Version:" (buffer-string)))))

    (warn "kubectl is not installed. Skipping test.")))

(ert-deftest listing-pods-returns-parsed-json ()
  (let* ((sample-response (f-read-text (f-join this-directory "get-pods-output.json")))
         (parsed-response (json-read-from-string sample-response))
         (cleanup-callback-called))

    (with-successful-response-at '("get" "pods" "-o" "json") sample-response
      (kubernetes-get-pods
       (lambda (response)
         (should (equal parsed-response response)))
       (lambda ()
         (setq cleanup-callback-called t))))
    (should cleanup-callback-called)))

(ert-deftest viewing-config-returns-parsed-json ()
  (let* ((sample-response (f-read-text (f-join this-directory "config-view-output.json")))
         (parsed-response (json-read-from-string sample-response))
         (cleanup-callback-called))
    (with-successful-response-at '("config" "view" "-o" "json") sample-response
      (kubernetes-config-view
       (lambda (response)
         (should (equal parsed-response response)))
       (lambda ()
         (setq cleanup-callback-called t))))
    (should cleanup-callback-called)))

(ert-deftest deleting-pod-succeeds ()
  (let ((pod-name "example-v3-4120544588-55kmw"))
    (with-successful-response-at '("delete" "pod" "example-pod" "-o" "name") "pod/example-v3-4120544588-55kmw"
      (kubernetes-delete-pod "example-pod"
                             (lambda (result)
                               (should (equal pod-name result)))))))

(ert-deftest deleting-pod-fails ()
  (let ((pod-name "example-v3-4120544588-55kmw")
        (on-error-called))
    (with-error-response-at '("delete" "pod" "example-pod" "-o" "name") "pod/example-v3-4120544588-55kmw"
      (kubernetes-delete-pod "example-pod"
                             (lambda (_)
                               (error "Unexpected success response"))
                             (lambda (result)
                               (setq on-error-called t))))
    (should on-error-called)))

(ert-deftest describing-pods ()
  (let ((pod-name "example-v3-4120544588-55kmw")
        (sample-response "foo bar baz")
        (on-success-called))
    (with-successful-response-at `("describe" "pod" ,pod-name) sample-response
      (kubernetes-kubectl-describe-pod pod-name
                                       (lambda (str)
                                         (setq on-success-called t)
                                         (should (equal sample-response str)))))
    (should on-success-called)))

(ert-deftest changing-current-context ()
  (let* ((context-name "context-name")
         (sample-response (format "Switched to context \"%s\".\n" context-name))
         (on-success-called))
    (with-successful-response-at (list "config" "use-context" context-name) sample-response
      (kubernetes-kubectl-config-use-context context-name
                                             (lambda (str)
                                               (setq on-success-called t)
                                               (should (equal context-name str)))))
    (should on-success-called)))

;;; kubectl-integration-test.el ends here
