;;; kubernetes-kubectl-test.el --- Tests for integration with kubectl  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'json)
(require 'noflet)
(require 'kubernetes-kubectl)
(declare-function test-helper-string-resource "test-helper.el")


;; Test helpers

(defmacro with-successful-response-at (expected-args response-string &rest forms)
  "Rebind `kubernetes-kubectl' to test handling of successful responses.

Asserts the the args parsed to that function are equal to EXPECTED-ARGS.

Executes the on-success callback with a buffer containing RESPONSE-STRING.

FORMS are the Elisp forms to be evaluated, in which `kubernetes-kubectl'
will be mocked."
  (declare (indent defun))
  `(noflet ((kubernetes-kubectl
             (_props _state args on-success &optional _on-error cleanup-cb)

             ;; Silence byte-compiler warnings
             this-fn

             
             (let ((buf (generate-new-buffer " test")))
               (with-current-buffer buf
                 (unwind-protect
                     (progn
                       (insert ,response-string)
                       (should (equal args ,expected-args))
                       (funcall on-success buf)))

                 (when cleanup-cb
                   (funcall cleanup-cb))))
             ;; Make and return a "dummy" process here solely to conform to the
             ;; expected return value of kubernetes-kubectl
             (make-process
              :name "dummy process"
              :command '("sleep" "0")
              :noquery t)))
     ,@forms))

(defmacro with-error-response-at (expected-args response-string &rest forms)
  "Rebind `kubernetes-kubectl' to test handling of failed responses.

Asserts the the args parsed to that function are equal to EXPECTED-ARGS.

Executes the on-error callback with a buffer containing RESPONSE-STRING.

FORMS are the Elisp forms to be evaluated, in which `kubernetes-kubectl'
will be mocked."
  (declare (indent 2))
  `(noflet ((kubernetes-kubectl
             (_props _state args _on-success &optional on-error cleanup-cb)

             ;; Silence byte-compiler warnings
             this-fn


             (let ((buf (generate-new-buffer " test")))
               (with-current-buffer buf
                 (unwind-protect
                     (progn
                       (insert ,response-string)
                       (should (equal args ,expected-args))
                       (should on-error)
                       (funcall on-error buf))

                   (when cleanup-cb
                     (funcall cleanup-cb)))))

             ;; Make and return a "dummy" process here solely to conform to the
             ;; expected return value of kubernetes-kubectl
             (make-process
              :name "dummy process"
              :command '("sleep" "0")
              :noquery t)))
     ,@forms))

(defconst kubernetes-kubectl-test-props
  '((message . ignore)
    (update-last-error . ignore)
    (overview-buffer-selected-p . ignore)
    (get-last-error . ignore)))


;; Subprocess calls

(ert-deftest kubernetes-kubectl-test--flags-from-state-applies-namespace ()
  (let ((state '((current-namespace . "foo"))))
    (should (equal '("--namespace=foo") (kubernetes-kubectl--flags-from-state state)))))

(ert-deftest kubernetes-kubectl-test--flags-from-state-applies-extra-flags ()
  (let ((state '((kubectl-flags . ("--foo=bar" "-x")))))
    (should (equal '("--foo=bar" "-x") (kubernetes-kubectl--flags-from-state state)))))

(ert-deftest kubernetes-kubectl-test--await-nohang ()
  (should (let ((kubernetes-kubectl-timeout-seconds 3))
            (kubernetes-kubectl-await
             (lambda (&rest callbacks) (funcall (car callbacks)))
             (lambda (&rest _args) t)))))

(ert-deftest kubernetes-kubectl-test--await-hang ()
  (should-not (let ((kubernetes-kubectl-timeout-seconds 3))
                (kubernetes-kubectl-await
                 #'ignore
                 (lambda (&rest _args) t)))))

(ert-deftest kubernetes-kubectl-test--running-kubectl-works ()
  (if (executable-find kubernetes-kubectl-executable)
      (let ((result-string (kubernetes-kubectl-await-on-async kubernetes-kubectl-test-props nil
                                            (lambda (props _ cb)
                                              (kubernetes-kubectl
                                               props
                                               nil
                                               '("version" "--client")
                                               (lambda (buf)
                                                 (with-current-buffer buf
                                                   (funcall cb (buffer-string)))))))))
        (should (string-prefix-p "Client Version:" result-string)))

    (warn "kubectl is not installed. Skipping test.")))

(defmacro define-refresh-tests (attr result)
  `(progn
     (ert-deftest ,(intern (format "kubernetes-kubectl-test--refresh-%s-now" attr)) ()
       (let ((sample-response (test-helper-string-resource
                               ,(format "get-%s-response.json" attr))))
         (with-successful-response-at '("get" ,attr "-o" "json") sample-response
           (should (equal (funcall #',(intern (format "kubernetes-%s-refresh-now" attr)))
                          ,result)))))
     (ert-deftest ,(intern (format "kubernetes-kubectl-test--refresh-%s" attr)) ()
       (let ((sample-response (test-helper-string-resource
                               ,(format "get-%s-response.json" attr))))
         (with-successful-response-at '("get" ,attr "-o" "json") sample-response
           (kubernetes-state-clear)
           (funcall #',(intern (format "kubernetes-%s-refresh" attr)))
           (should (equal (-let* (((&alist 'items)
                                   (kubernetes-state--get (kubernetes-state) (intern ,attr))))
                            (seq-map (lambda (item)
                                       (-let* (((&alist 'metadata (&alist 'name)) item)) name))
                                     items))
                          ,result)))))))

(define-refresh-tests "pods" '("example-svc-v3-1603416598-2f9lb"
                               "example-svc-v4-1603416598-2f9lb"
                               "example-svc-v5-1603416598-2f9lb"
                               "example-svc-v6-1603416598-2f9lb"))

;; contexts

(ert-deftest kubernetes-kubectl-test-refresh-contexts ()
  (let ((sample-response (test-helper-string-resource "config-view-response.json")))
    (with-successful-response-at '("config" "view" "-o" "json") sample-response
      (kubernetes-state-clear)
      (kubernetes-contexts-refresh)
      (should (equal (-let* (((&alist 'contexts)
                              (kubernetes-state-contexts (kubernetes-state))))
                       (seq-map (lambda (ctx)
                                  (-let* (((&alist 'name) ctx)) name))
                                contexts))
                     '("example-dev" "example-prod"))))))

(ert-deftest kubernetes-kubectl-test--viewing-config-returns-parsed-json ()
  (let* ((sample-response (test-helper-string-resource "config-view-response.json"))
         (parsed-response (json-read-from-string sample-response))
         (cleanup-callback-called))
    (with-successful-response-at '("config" "view" "-o" "json") sample-response
      (kubernetes-kubectl-config-view kubernetes-kubectl-test-props
                    nil
                    (lambda (response)
                      (should (equal parsed-response response)))
                    (lambda ()
                      (setq cleanup-callback-called t))))
    (should cleanup-callback-called)))


(define-refresh-tests "services" '("example-svc-1" "example-svc-2"))


;; Describe pod

(ert-deftest kubernetes-kubectl-test--describing-pods ()
  (let ((pod-name "example-v3-4120544588-55kmw")
        (sample-response "foo bar baz")
        (on-success-called))
    (with-successful-response-at `("describe" "pod" ,pod-name) sample-response
      (kubernetes-kubectl-describe-pod kubernetes-kubectl-test-props
                     nil
                     pod-name
                     (lambda (str)
                       (setq on-success-called t)
                       (should (equal sample-response str)))))
    (should on-success-called)))


;; Use context

(ert-deftest kubernetes-kubectl-test--changing-current-context ()
  (let* ((context-name "context-name")
         (sample-response (format "Switched to context \"%s\".\n" context-name))
         (on-success-called))
    (with-successful-response-at (list "config" "use-context" context-name) sample-response
      (kubernetes-kubectl-config-use-context kubernetes-kubectl-test-props
                           nil
                           context-name
                           (lambda (str)
                             (setq on-success-called t)
                             (should (equal context-name str)))))
    (should on-success-called)))


(define-refresh-tests "namespaces" '("ns1" "ns2"))

(ert-deftest kubernetes-kubectl-test--get--returns-parsed-json ()
  (--each '(("configmaps" "get-configmaps-response.json")
            ("namespace" "get-namespaces-response.json")
            ("pods" "get-pods-response.json")
            ("secrets" "get-secrets-response.json")
            ("services" "get-services-response.json")
            ("deployments" "get-deployments-response.json")
            ("jobs" "get-jobs-response.json"))
    (-let* (((type fake-response) it)
            (sample-response (test-helper-string-resource fake-response))
            (parsed-response (json-read-from-string sample-response))
            (on-success-called)
            (cleanup-callback-called))
      (with-successful-response-at
       `("get" ,type "-o" "json")
       sample-response
       (kubernetes-kubectl-get
        type
        kubernetes-kubectl-test-props
        nil
        (lambda (response)
          (setq on-success-called t)
          (should (equal parsed-response response)))
        (lambda ()
          (setq cleanup-callback-called t))))
      (should on-success-called)
      (should cleanup-callback-called))))

(define-refresh-tests "configmaps" '("example-configmap-1" "example-configmap-2"))

;; Delete configmap

(ert-deftest kubernetes-kubectl-test--delete--succeeds ()
  (--each '(("configmap" "example-configmap")
            ("foo" "example-foo"))
    (-let* (((type name) it))
      (with-successful-response-at
       `("delete" ,type ,name "-o" "name")
       (s-join "/" (list type name))
       (kubernetes-kubectl-delete type name kubernetes-kubectl-test-props nil
                                  (lambda (result)
                                    (should (equal name result))))))))

(ert-deftest kubernetes-kubectl-test--delete--fails ()
  (--each '(("configmap" "example-configmap"))
    (-let* ((on-error-called)
            ((type name) it))
      (with-error-response-at
       `("delete" ,type ,name "-o" "name")
       (s-join "/" (list type name))
       (kubernetes-kubectl-delete type name kubernetes-kubectl-test-props nil
                                  (lambda (_)
                                    (error "Unexpected success response"))
                                  (lambda (_)
                                    (setq on-error-called t))))
      (should on-error-called))))

(define-refresh-tests "secrets" '("example-secret-1" "example-secret-2"))

;; Delete secret

(define-refresh-tests "deployments" '("deployment-1" "deployment-2"))

(define-refresh-tests "jobs" '("example-job-1" "example-job-2"))

;; Delete job

;; Delete deployment

;; Error handler

(ert-deftest kubernetes-kubectl-test--error-handler-writes-messages-when-overview-buffer-not-selected ()
  (let* ((message-called-p)
         (props
          (append `((message . ,(lambda (&rest _) (setq message-called-p t))))
                  kubernetes-kubectl-test-props)))

    (kubernetes-kubectl--default-error-handler props "")
    (should message-called-p)))

(ert-deftest kubernetes-kubectl-test--error-handler-does-not-write-message-if-overview-buffer-selected ()
  (let* ((message-called-p)
         (props
          (append `((message . ,(lambda (&rest _) (setq message-called-p t)))
                    (overview-buffer-selected-p . (lambda (&rest _) t)))
                  kubernetes-kubectl-test-props)))

    (kubernetes-kubectl--default-error-handler props "")
    (should-not message-called-p)))

(ert-deftest kubernetes-kubectl-test--error-handler-write-message ()
  (let* ((props kubernetes-props))
    (setf (alist-get 'message props) 'format)
    (kubernetes-props-update-last-error props "bar" "foo" (current-time))
    (should (string= (kubernetes-kubectl--default-error-handler props "")
                     "foo: bar"))))

(ert-deftest kubernetes-kubectl-test--error-handler-does-not-write-message-if-process-was-sigkilled ()
  (let* ((message-called-p)
         (props (append `((message . ,(lambda (&rest _) (setq message-called-p t))))
                        kubernetes-kubectl-test-props)))

    (kubernetes-kubectl--default-error-handler props "killed: 9")
    (should-not message-called-p)))

;; Edit resource

(ert-deftest kubernetes-kubectl-test--edit-resource-succeeds ()
  (let ((deployment-name "example-deployment"))
    (with-successful-response-at
     (list "edit" "deployment" deployment-name) deployment-name
     (kubernetes-kubectl-edit-resource kubernetes-kubectl-test-props
                                       nil
                                       "deployment"
                                       deployment-name
                                       (lambda (buf)
                                         (let ((s (with-current-buffer buf (buffer-string))))
                                           (should (equal deployment-name s))))))))

(ert-deftest kubernetes-kubectl-test--edit-resource-fails ()
  (let ((on-error-called)
        (deployment-name "example-deployment"))
    (with-error-response-at
     (list "edit" "deployment" "example-deployment") deployment-name
     (kubernetes-kubectl-edit-resource kubernetes-kubectl-test-props
                                       nil
                                       "deployment"
                                       deployment-name
                                       (lambda (_)
                                         (error "Unexpected success response"))
                                       (lambda (_)
                                         (setq on-error-called t))))
    (should on-error-called)))

;;; kubernetes-kubectl-test.el ends here
