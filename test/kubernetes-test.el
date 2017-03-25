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
  `(noflet ((kubernetes--kubectl (args on-success)
                       (let ((buf (generate-new-buffer " test")))
                         (with-current-buffer buf
                           (insert ,response-string)
                           (should (equal args ,expected-args))
                           (funcall on-success buf)))))
     ,form))


;; Test cases

(ert-deftest running-kubectl-works ()
  (if (executable-find kubernetes-kubectl-executable)
      (let* ((callback (lambda (buf)
                         (with-current-buffer buf
                           (should (string-prefix-p "Client Version:" (buffer-string))))))

             (process (kubernetes--kubectl '("version" "--client") callback)))

        (while (equal 'run (process-status process))
          (sleep-for 0.001)))

    (warn "kubectl is not installed. Skipping test.")))


(ert-deftest listing-pods-returns-parsed-json ()
  (let* ((sample-response (f-read-text (f-join this-directory "get-pods-output.json")))
         (parsed-response (json-read-from-string sample-response)))

    (with-successful-response-at '("get" "pods" "-o" "json") sample-response
      (kubernetes-get-pods
       (lambda (response)
         (should (equal parsed-response response)))))))

(ert-deftest listing-current-contexts ()
  (let ((sample-response "example-context\n"))
    (with-successful-response-at '("config" "current-context") sample-response
      (kubernetes-config-current-context
       (lambda (response)
         (should (equal "example-context" response)))))))

(provide 'kubernetes-test)

;;; kubernetes-test.el ends here
