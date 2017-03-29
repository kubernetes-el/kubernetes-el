;;; kubernetes-test.el --- Tests for kubernetes.el  -*- lexical-binding: t; -*-
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

;; Test resources

;; Test helpers

(defmacro with-successful-response-at (expected-args response-string form)
  "Rebind `kubernetes--kubectl' to test handling of successful responses.

Asserts the the args parsed to that function are equal to EXPECTED-ARGS.

Executes the on-success callback with a buffer containing RESPONSE-STRING.

FORM is the Elisp form to be evaluated, in which `kubernetes--kubectl'
will be mocked."
  (declare (indent 2))
  `(noflet ((kubernetes--kubectl (args on-success &optional on-error)
                       (let ((buf (generate-new-buffer " test")))
                         (with-current-buffer buf
                           (insert ,response-string)
                           (should (equal args ,expected-args))
                           (funcall on-success buf)))))
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
         (parsed-response (json-read-from-string sample-response)))

    (with-successful-response-at '("get" "pods" "-o" "json") sample-response
      (kubernetes-get-pods
       (lambda (response)
         (should (equal parsed-response response)))))))

(ert-deftest viewing-config-returns-parsed-json ()
  (let* ((sample-response (f-read-text (f-join this-directory "config-view-output.json")))
         (parsed-response (json-read-from-string sample-response)))
    (with-successful-response-at '("config" "view" "-o" "json") sample-response
      (kubernetes-config-view
       (lambda (response)
         (should (equal parsed-response response)))))))

(ert-deftest deleting-pod-succeeds ()
  (let* ((pod-name "example-v3-4120544588-55kmw")
         (response (concat "pod/" pod-name))
         response-buffer)
    (with-successful-response-at '("delete" "pod" "example-pod" "-o" "name") "pod/example-v3-4120544588-55kmw"
      (kubernetes-delete-pod "example-pod"
                   (lambda (result)
                     (should (equal pod-name result)))))))

;;; kubernetes-test.el ends here
