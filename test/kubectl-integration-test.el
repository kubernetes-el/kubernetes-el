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

;; Subprocess calls

(ert-deftest running-kubectl-works ()
  (if (executable-find kubernetes-kubectl-executable)
      (let ((result-string (kubernetes--await-on-async
                            (lambda (cb)
                              (kubernetes--kubectl '("version" "--client")
                                                   (lambda (buf)
                                                     (with-current-buffer buf
                                                       (funcall cb (buffer-string)))))))))
        (should (string-prefix-p "Client Version:" result-string)))

    (warn "kubectl is not installed. Skipping test.")))


;; Get pods

(ert-deftest get-pods-returns-parsed-json ()
  (let* ((sample-response (f-read-text (f-join this-directory "get-pods-output.json")))
         (parsed-response (json-read-from-string sample-response))
         (cleanup-callback-called))

    (with-successful-response-at '("get" "pods" "-o" "json") sample-response
      (kubernetes--kubectl-get-pods
       (lambda (response)
         (should (equal parsed-response response)))
       (lambda ()
         (setq cleanup-callback-called t))))
    (should cleanup-callback-called)))

(ert-deftest get-pods-returning-no-response ()
  (let* ((err-response (f-read-text (f-join this-directory "get-pods-no-resources-response.txt")))
         (sans-first-line (string-join (cdr (split-string err-response (rx (any "\n")))) "\n"))
         (parsed-response (json-read-from-string sans-first-line)))

    (with-successful-response-at '("get" "pods" "-o" "json") err-response
      (kubernetes--kubectl-get-pods
       (lambda (response)
         (should (equal parsed-response response)))))))

(ert-deftest get-pods-applies-current-namespace ()
  (let ((kubernetes--current-namespace "foo"))
    (with-successful-response-at `("get" "pods" "-o" "json" "--namespace=foo")
        "{}"
      (kubernetes--kubectl-get-pods #'ignore))))

(ert-deftest viewing-config-returns-parsed-json ()
  (let* ((sample-response (f-read-text (f-join this-directory "config-view-output.json")))
         (parsed-response (json-read-from-string sample-response))
         (cleanup-callback-called))
    (with-successful-response-at '("config" "view" "-o" "json") sample-response
      (kubernetes--kubectl-config-view
       (lambda (response)
         (should (equal parsed-response response)))
       (lambda ()
         (setq cleanup-callback-called t))))
    (should cleanup-callback-called)))


;; Delete pod

(ert-deftest deleting-pod-succeeds ()
  (let ((pod-name "example-v3-4120544588-55kmw"))
    (with-successful-response-at '("delete" "pod" "example-pod" "-o" "name") "pod/example-v3-4120544588-55kmw"
      (kubernetes--kubectl-delete-pod "example-pod"
                                      (lambda (result)
                                        (should (equal pod-name result)))))))

(ert-deftest deleting-pod-fails ()
  (let ((pod-name "example-v3-4120544588-55kmw")
        (on-error-called))
    (with-error-response-at '("delete" "pod" "example-pod" "-o" "name") "pod/example-v3-4120544588-55kmw"
      (kubernetes--kubectl-delete-pod "example-pod"
                                      (lambda (_)
                                        (error "Unexpected success response"))
                                      (lambda (result)
                                        (setq on-error-called t))))
    (should on-error-called)))

(ert-deftest deleting-pod-applies-current-namespace ()
  (let* ((pod-name "example-v3-4120544588-55kmw")
         (kubernetes--current-namespace "foo"))
    (with-successful-response-at `("delete" "pod" ,pod-name "-o" "name"
                                   "--namespace=foo")
        "pod/example-v3-4120544588-55kmw"
      (kubernetes--kubectl-delete-pod pod-name #'ignore))))


;; Describe pod

(ert-deftest describing-pods ()
  (let ((pod-name "example-v3-4120544588-55kmw")
        (sample-response "foo bar baz")
        (on-success-called))
    (with-successful-response-at `("describe" "pod" ,pod-name) sample-response
      (kubernetes--kubectl-describe-pod pod-name
                                        (lambda (str)
                                          (setq on-success-called t)
                                          (should (equal sample-response str)))))
    (should on-success-called)))

(ert-deftest describing-pod-applies-current-namespace ()
  (let* ((pod-name "example-v3-4120544588-55kmw")
         (kubernetes--current-namespace "foo"))
    (with-successful-response-at `("describe" "pod" ,pod-name "--namespace=foo")
        ""
      (kubernetes--kubectl-describe-pod pod-name #'ignore))))


;; Use context

(ert-deftest changing-current-context ()
  (let* ((context-name "context-name")
         (sample-response (format "Switched to context \"%s\".\n" context-name))
         (on-success-called))
    (with-successful-response-at (list "config" "use-context" context-name) sample-response
      (kubernetes--kubectl-config-use-context context-name
                                              (lambda (str)
                                                (setq on-success-called t)
                                                (should (equal context-name str)))))
    (should on-success-called)))


;; Get namespaces

(ert-deftest getting-namespaces ()
  (let* ((sample-response (f-read-text (f-join this-directory "get-namespaces-output.json")))
         (parsed-response (json-read-from-string sample-response))
         (on-success-called)
         (cleanup-callback-called))
    (with-successful-response-at '("get" "namespaces" "-o" "json") sample-response
      (kubernetes--kubectl-get-namespaces (lambda (response)
                                            (setq on-success-called t)
                                            (should (equal parsed-response response)))
                                          (lambda ()
                                            (setq cleanup-callback-called t))))
    (should on-success-called)
    (should cleanup-callback-called)))


;; Get configmaps

(ert-deftest get-configmaps-returns-parsed-json ()
  (let* ((sample-response (f-read-text (f-join this-directory "get-configmaps-output.json")))
         (parsed-response (json-read-from-string sample-response))
         (cleanup-callback-called))

    (with-successful-response-at '("get" "configmaps" "-o" "json") sample-response
      (kubernetes--kubectl-get-configmaps
       (lambda (response)
         (should (equal parsed-response response)))
       (lambda ()
         (setq cleanup-callback-called t))))
    (should cleanup-callback-called)))

(ert-deftest get-configmaps-returning-no-response ()
  (let* ((empty-response (f-read-text (f-join this-directory "get-configmaps-no-resources-response.json")))
         (parsed-response (json-read-from-string empty-response)))
    (with-successful-response-at '("get" "configmaps" "-o" "json") empty-response
      (kubernetes--kubectl-get-configmaps
       (lambda (response)
         (should (equal parsed-response response)))))))

(ert-deftest get-configmaps-applies-current-namespace ()
  (let ((kubernetes--current-namespace "foo"))
    (with-successful-response-at `("get" "configmaps" "-o" "json" "--namespace=foo")
        "{}"
      (kubernetes--kubectl-get-configmaps #'ignore))))


;; Delete configmap

(ert-deftest deleting-configmap-succeeds ()
  (let ((configmap-name "example-config"))
    (with-successful-response-at '("delete" "configmap" "example-configmap" "-o" "name") "configmap/example-config"
      (kubernetes--kubectl-delete-configmap "example-configmap"
                                            (lambda (result)
                                              (should (equal configmap-name result)))))))

(ert-deftest deleting-configmap-fails ()
  (let ((configmap-name "example-config")
        (on-error-called))
    (with-error-response-at '("delete" "configmap" "example-configmap" "-o" "name") "configmap/example-config"
      (kubernetes--kubectl-delete-configmap "example-configmap"
                                            (lambda (_)
                                              (error "Unexpected success response"))
                                            (lambda (result)
                                              (setq on-error-called t))))
    (should on-error-called)))

(ert-deftest deleting-configmap-applies-current-namespace ()
  (let* ((configmap-name "example-config")
         (kubernetes--current-namespace "foo"))
    (with-successful-response-at `("delete" "configmap" ,configmap-name "-o" "name"
                                   "--namespace=foo")
        "configmap/example-config"
      (kubernetes--kubectl-delete-configmap configmap-name #'ignore))))


;; Get secrets

(ert-deftest get-secrets-returns-parsed-json ()
  (let* ((sample-response (f-read-text (f-join this-directory "get-secrets-response.json")))
         (parsed-response (json-read-from-string sample-response))
         (cleanup-callback-called))

    (with-successful-response-at '("get" "secrets" "-o" "json") sample-response
      (kubernetes--kubectl-get-secrets
       (lambda (response)
         (should (equal parsed-response response)))
       (lambda ()
         (setq cleanup-callback-called t))))
    (should cleanup-callback-called)))

(ert-deftest get-secrets-returning-no-response ()
  (let* ((empty-response (f-read-text (f-join this-directory "get-secrets-no-resources-response.json")))
         (parsed-response (json-read-from-string empty-response)))
    (with-successful-response-at '("get" "secrets" "-o" "json") empty-response
      (kubernetes--kubectl-get-secrets
       (lambda (response)
         (should (equal parsed-response response)))))))

(ert-deftest get-secrets-applies-current-namespace ()
  (let ((kubernetes--current-namespace "foo"))
    (with-successful-response-at `("get" "secrets" "-o" "json" "--namespace=foo")
        "{}"
      (kubernetes--kubectl-get-secrets #'ignore))))


;; Delete configmap

(ert-deftest deleting-secret-succeeds ()
  (let ((secret-name "example-config"))
    (with-successful-response-at '("delete" "secret" "example-secret" "-o" "name") "secret/example-config"
      (kubernetes--kubectl-delete-secret "example-secret"
                                            (lambda (result)
                                              (should (equal secret-name result)))))))

(ert-deftest deleting-secret-fails ()
  (let ((secret-name "example-config")
        (on-error-called))
    (with-error-response-at '("delete" "secret" "example-secret" "-o" "name") "secret/example-config"
      (kubernetes--kubectl-delete-secret "example-secret"
                                         (lambda (_)
                                           (error "Unexpected success response"))
                                         (lambda (result)
                                           (setq on-error-called t))))
    (should on-error-called)))

(ert-deftest deleting-secret-applies-current-namespace ()
  (let* ((secret-name "example-config")
         (kubernetes--current-namespace "foo"))
    (with-successful-response-at `("delete" "secret" ,secret-name "-o" "name"
                                   "--namespace=foo")
        "secret/example-config"
      (kubernetes--kubectl-delete-secret secret-name #'ignore))))


;; Get services

(ert-deftest get-services-returns-parsed-json ()
  (let* ((sample-response (f-read-text (f-join this-directory "get-services-response.json")))
         (parsed-response (json-read-from-string sample-response))
         (cleanup-callback-called))

    (with-successful-response-at '("get" "services" "-o" "json") sample-response
      (kubernetes--kubectl-get-services
       (lambda (response)
         (should (equal parsed-response response)))
       (lambda ()
         (setq cleanup-callback-called t))))
    (should cleanup-callback-called)))

(ert-deftest get-services-returning-no-response ()
  (let* ((err-response (f-read-text (f-join this-directory "get-services-no-resources-response.txt")))
         (sans-first-line (string-join (cdr (split-string err-response (rx (any "\n")))) "\n"))
         (parsed-response (json-read-from-string sans-first-line)))
    (with-successful-response-at '("get" "services" "-o" "json") err-response
      (kubernetes--kubectl-get-services
       (lambda (response)
         (should (equal parsed-response response)))))))

(ert-deftest get-services-applies-current-namespace ()
  (let ((kubernetes--current-namespace "foo"))
    (with-successful-response-at `("get" "services" "-o" "json" "--namespace=foo")
        "{}"
      (kubernetes--kubectl-get-services #'ignore))))


;;; kubectl-integration-test.el ends here
