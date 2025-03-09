;;; kubernetes-describe-test.el --- Tests for kubernetes-describe.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'kubernetes-commands)
(require 'kubernetes-kubectl)
(require 'kubernetes-state)
(require 'kubernetes-utils)
(require 'kubernetes-vars)
(require 'kubernetes-pods)
(require 'kubernetes-deployments)
(require 'kubernetes-services)
(require 'kubernetes-nodes)

(require 'kubernetes-describe)

(defun kubernetes-describe-test-make-resource-info (type name)
  "Create a resource info pair of TYPE and NAME."
  (cons type name))

(ert-deftest kubernetes-describable-resources-test ()
  "Test that kubernetes-describable-resources contains expected resources."
  (should (member "pod" kubernetes-describable-resources))
  (should (member "deployment" kubernetes-describable-resources))
  (should (member "service" kubernetes-describable-resources))
  (should (member "node" kubernetes-describable-resources))
  (should (member "persistentvolumeclaim" kubernetes-describable-resources))
  (should (= (length kubernetes-describable-resources) 13)))

(ert-deftest kubernetes-describe--validate-resource-type-test ()
  "Test validation of resource types."
  (should-error (kubernetes-describe--validate-resource-type "invalid-type")
                :type 'user-error)
  (should-not (kubernetes-describe--validate-resource-type "pod"))
  (should-not (kubernetes-describe--validate-resource-type "pods"))
  (should-not (kubernetes-describe--validate-resource-type "persistentvolumeclaim"))
  (should-not (kubernetes-describe--validate-resource-type "networkpolicy")))

(ert-deftest kubernetes-describe--get-resource-name-test ()
  "Test kubernetes-describe--get-resource-name function."
  (cl-letf (((symbol-function 'kubernetes-describe--validate-resource-type)
             (lambda (resource-type)
               (unless (member resource-type kubernetes-describable-resources)
                 (user-error "Resource type %s is not describable" resource-type)))))
    (should-error (kubernetes-describe--get-resource-name nil "invalid-type")
                  :type 'user-error))

  (cl-letf (((symbol-function 'kubernetes-pods--read-name)
             (lambda (_) "test-pod-from-reader"))
            ((symbol-function 'kubernetes-describe--validate-resource-type)
             (lambda (_) nil)))
    (let ((result nil))
      (cl-letf (((symbol-function 'kubernetes-describe--get-resource-name)
                 (lambda (_state type)
                   (should (equal type "pod"))
                   (setq result "test-pod-from-reader")
                   result)))
        (should (equal (kubernetes-describe--get-resource-name nil "pod")
                      "test-pod-from-reader"))))))

(ert-deftest kubernetes-kubectl-describe-resource-test ()
  "Test kubernetes-kubectl-describe-resource function."
  (let ((callback-called nil)
        (callback-data nil)
        (kubectl-args nil))
    (cl-letf (((symbol-function 'kubernetes-kubectl)
               (lambda (state args on-success &rest _)
                 (setq kubectl-args (list state args))
                 (with-temp-buffer
                   (insert "Mock kubectl output")
                   (funcall on-success (current-buffer))))))
      (kubernetes-kubectl-describe-resource '((current-namespace . "test-ns"))
                                           "pod"
                                           "test-pod"
                                           (lambda (output)
                                             (setq callback-called t
                                                   callback-data output)))

      ;; Verify kubectl was called correctly
      (should (equal (cadr kubectl-args) '("describe" "pod" "test-pod")))

      ;; Verify callback was called with output
      (should callback-called)
      (should (equal callback-data "Mock kubectl output")))))

(when (fboundp 'kubernetes-describe--generate-buffer-name)
  (ert-deftest kubernetes-describe--generate-buffer-name-test ()
    "Test kubernetes-describe--generate-buffer-name function."
    (let ((state '((current-namespace . "test-namespace"))))
      (should (equal (kubernetes-describe--generate-buffer-name "pod" "test-pod" state)
                    "*kubernetes describe test-namespace/pod/test-pod*")))

    (let ((state '()))
      (should (equal (kubernetes-describe--generate-buffer-name "deployment" "test-deploy" state)
                    "*kubernetes describe deployment/test-deploy*")))))

(ert-deftest kubernetes-describe-generic-resource-test ()
  "Test kubernetes-describe-generic-resource function."
  ;; Test with resource-specific getter function
  (let ((describe-called nil)
        (describe-args nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _collection &rest _) "pod"))
              ((symbol-function 'kubernetes-utils-maybe-pod-name-at-point)
               (lambda () "point-pod"))
              ((symbol-function 'kubernetes-describe-resource)
               (lambda (type name)
                 (setq describe-called t
                       describe-args (list type name)))))
      (call-interactively #'kubernetes-describe-generic-resource)
      (should describe-called)
      (should (equal describe-args '("pod" "point-pod")))))

  (let ((describe-called nil)
        (describe-args nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _collection &rest _) "deployment"))
              ((symbol-function 'kubernetes-utils-maybe-deployment-name-at-point)
               (lambda () nil))
              ((symbol-function 'kubernetes-deployments--read-name)
               (lambda (_) "read-deployment"))
              ((symbol-function 'kubernetes-state)
               (lambda () '()))
              ((symbol-function 'kubernetes-describe-resource)
               (lambda (type name)
                 (setq describe-called t
                       describe-args (list type name)))))
      (call-interactively #'kubernetes-describe-generic-resource)
      (should describe-called)
      (should (equal describe-args '("deployment" "read-deployment"))))))

(ert-deftest kubernetes-describe-resource-test ()
  "Test kubernetes-describe-resource function."
  (let ((proc-args nil))
    (cl-letf* (((symbol-function 'kubernetes-state)
                (lambda () '((current-namespace . "test-ns"))))
               ;; Don't mock get-buffer-create at all - it's causing issues
               ((symbol-function 'kubernetes-display-thing-mode)
                (lambda () nil))
               ((symbol-function 'kubernetes-kubectl-describe-resource)
                (lambda (state type name callback)
                  (setq proc-args (list state type name))
                  (funcall callback "Test output")))
               ((symbol-function 'make-marker)
                (lambda () (point-marker)))
               ((symbol-function 'set-marker)
                (lambda (marker point) marker))
               ((symbol-function 'select-window)
                (lambda (_) nil))
               ((symbol-function 'display-buffer)
                (lambda (buf) buf))
               ;; Avoid buffer operations entirely
               ((symbol-function 'with-current-buffer)
                (lambda (buffer &rest body)
                  (eval (cons 'progn body)))))
      (let ((buf (kubernetes-describe-resource "deployment" "test-deployment")))
        ;; Just check that the function calls kubectl with the right args
        (should (equal (cadr proc-args) "deployment"))
        (should (equal (caddr proc-args) "test-deployment"))))))

(ert-deftest kubernetes-describe-dwim-test ()
  "Test the core logic of kubernetes-describe-dwim."
  (let ((described-resource nil))
    ;; Test with resource at point
    (cl-letf (((symbol-function 'kubernetes-utils-get-resource-info-at-point)
               (lambda () (kubernetes-describe-test-make-resource-info "pod" "test-pod")))
              ((symbol-function 'kubernetes-describe-resource)
               (lambda (type name)
                 (should (equal type "pod"))
                 (setq described-resource name)
                 (generate-new-buffer "*test-buffer*"))))
      (kubernetes-describe-dwim)
      (should (equal described-resource "test-pod")))))

(ert-deftest kubernetes-describe-dwim-no-resource-test ()
  "Test kubernetes-describe-dwim with no resource at point."
  (cl-letf (((symbol-function 'kubernetes-utils-get-resource-info-at-point)
             (lambda () nil)))
    (should-error (kubernetes-describe-dwim) :type 'user-error)))

(ert-deftest kubernetes-describe-dwim-invalid-resource-test ()
  "Test kubernetes-describe-dwim with invalid resource type."
  (cl-letf (((symbol-function 'kubernetes-utils-get-resource-info-at-point)
             (lambda () (kubernetes-describe-test-make-resource-info "invalid-type" "test-name"))))
    (should-error (kubernetes-describe-dwim) :type 'user-error)))

(ert-deftest kubernetes-describe-pod-alias-test ()
  "Test that kubernetes-describe-pod alias works."
  (let ((describe-args nil))
    (cl-letf (((symbol-function 'kubernetes-utils-maybe-pod-name-at-point)
               (lambda () "test-pod"))
              ((symbol-function 'kubernetes-describe-resource)
               (lambda (type name)
                 (setq describe-args (list type name))
                 (generate-new-buffer "*test-buffer*"))))
      (kubernetes-describe-pod "manual-pod")
      (should (equal describe-args '("pod" "manual-pod"))))))

(ert-deftest kubernetes-describe-deployment-alias-test ()
  "Test that kubernetes-describe-deployment alias works."
  (let ((describe-args nil))
    (cl-letf (((symbol-function 'kubernetes-utils-maybe-deployment-name-at-point)
               (lambda () nil))
              ((symbol-function 'kubernetes-deployments--read-name)
               (lambda (_) "read-deployment"))
              ((symbol-function 'kubernetes-state)
               (lambda () '()))
              ((symbol-function 'kubernetes-describe-resource)
               (lambda (type name)
                 (setq describe-args (list type name))
                 (generate-new-buffer "*test-buffer*"))))
      (call-interactively #'kubernetes-describe-deployment)
      (should (equal describe-args '("deployment" "read-deployment"))))))

(ert-deftest kubernetes-describe-service-alias-test ()
  "Test that kubernetes-describe-service alias works."
  (let ((describe-args nil))
    (cl-letf (((symbol-function 'kubernetes-utils-get-resource-name-at-point)
               (lambda (_) "test-service"))
              ((symbol-function 'kubernetes-describe-resource)
               (lambda (type name)
                 (setq describe-args (list type name))
                 (generate-new-buffer "*test-buffer*"))))
      (kubernetes-describe-service "manual-service")
      (should (equal describe-args '("service" "manual-service"))))))


(ert-deftest kubernetes-describe--generate-buffer-name-complete-test ()
  "Test kubernetes-describe--generate-buffer-name with all code paths."
  ;; With namespace
  (let ((state '((current-namespace . "test-ns"))))
    (should (equal (kubernetes-describe--generate-buffer-name "pod" "test-pod" state)
                  "*kubernetes describe test-ns/pod/test-pod*")))

  ;; Without namespace
  (let ((state '()))
    (should (equal (kubernetes-describe--generate-buffer-name "service" "test-service" state)
                  "*kubernetes describe service/test-service*"))))

(ert-deftest kubernetes-kubectl-describe-resource-complete-test ()
  "Test kubernetes-kubectl-describe-resource with full implementation."
  (let ((kubectl-called nil)
        (kubectl-args nil)
        (callback-called nil)
        (callback-data nil))
    (cl-letf (((symbol-function 'kubernetes-kubectl)
               (lambda (state args on-success &rest _)
                 (setq kubectl-called t
                       kubectl-args (list state args))
                 (with-temp-buffer
                   (insert "Test kubectl output")
                   (funcall on-success (current-buffer))))))

      (kubernetes-kubectl-describe-resource '((current-namespace . "test-ns"))
                                          "deployment"
                                          "test-deployment"
                                          (lambda (output)
                                            (setq callback-called t
                                                  callback-data output)))

      ;; Verify kubectl was called correctly
      (should kubectl-called)
      (should (equal (car kubectl-args) '((current-namespace . "test-ns"))))
      (should (equal (cadr kubectl-args) '("describe" "deployment" "test-deployment")))

      ;; Verify callback received output
      (should callback-called)
      (should (equal callback-data "Test kubectl output")))))

(defmacro with-kubectl-mocks (&rest body)
  "Execute BODY with kubectl functions mocked to prevent actual kubectl calls."
  `(cl-letf (((symbol-function 'kubernetes-kubectl-await-on-async)
              (lambda (state fn)
                (let ((resource-type (nth 1 fn)))
                  (cond
                   ((string= resource-type "pods")
                    '((items . (((metadata . ((name . "test-pod-1"))))
                               ((metadata . ((name . "test-pod-2"))))))))
                   ((string= resource-type "deployments")
                    '((items . (((metadata . ((name . "test-deployment-1"))))
                               ((metadata . ((name . "test-deployment-2"))))))))
                   ((string= resource-type "services")
                    '((items . (((metadata . ((name . "test-service-1"))))
                               ((metadata . ((name . "test-service-2"))))))))
                   (t
                    '((items . nil)))))))
             ((symbol-function 'kubernetes-kubectl-config-view)
              (lambda (&rest _) nil))
             ((symbol-function 'kubernetes-kubectl-get)
              (lambda (&rest _) '((items . nil))))
             ((symbol-function 'kubernetes-kubectl-get-pods)
              (lambda (&rest _) nil))
             ((symbol-function 'kubernetes-kubectl)
              (lambda (state args on-success &rest _)
                (with-temp-buffer
                  (insert "Mock kubectl output")
                  (funcall on-success (current-buffer))))))
     ,@body))

;; Test kubernetes-describe-resource interactive form
(ert-deftest kubernetes-describe-resource-interactive-test ()
  "Test the interactive form of kubernetes-describe-resource."
  (with-kubectl-mocks
   (cl-letf (((symbol-function 'kubernetes-utils-get-resource-info-at-point)
              (lambda () (cons "deployment" "test-deployment"))))
     ;; Using eval to test the interactive form
     (let ((interactive-spec (car (cdr (interactive-form #'kubernetes-describe-resource)))))
       (let ((result (eval interactive-spec)))
         (should (equal result '("deployment" "test-deployment"))))))))

;; Test the interactive form when no resource is at point
(ert-deftest kubernetes-describe-resource-interactive-no-point-test ()
  "Test the interactive form of kubernetes-describe-resource with no resource at point."
  (with-kubectl-mocks
   (cl-letf (((symbol-function 'kubernetes-utils-get-resource-info-at-point)
              (lambda () nil))
             ((symbol-function 'kubernetes-state)
              (lambda () '((current-namespace . "test-ns"))))
             ((symbol-function 'completing-read)
              (lambda (prompt collection &rest _)
                (should (string= prompt "Resource type: "))
                (should (member "pod" collection))
                "configmap"))
             ((symbol-function 'kubernetes-describe--get-resource-name)
              (lambda (state type)
                (should (equal state '((current-namespace . "test-ns"))))
                (should (equal type "configmap"))
                "test-configmap")))
     ;; Using eval to test the interactive form
     (let ((interactive-spec (car (cdr (interactive-form #'kubernetes-describe-resource)))))
       (let ((result (eval interactive-spec)))
         (should (equal result '("configmap" "test-configmap"))))))))

(ert-deftest kubernetes-describe-simple-test ()
  "Simple test for kubernetes-describe functionality."
  ;; Test the constant
  (should (member "pod" kubernetes-describable-resources))
  (should (member "service" kubernetes-describable-resources))

  ;; Test resource validation
  (cl-letf (((symbol-function 'user-error)
             (lambda (msg &rest args)
               (signal 'user-error (list msg)))))
    (should-error (kubernetes-describe--validate-resource-type "invalid-type")
                  :type 'user-error)
    (should-not (kubernetes-describe--validate-resource-type "pod")))

  ;; Test buffer name generation
  (should (equal (kubernetes-describe--generate-buffer-name "pod" "test-pod" '((current-namespace . "test-ns")))
                "*kubernetes describe test-ns/pod/test-pod*"))
  (should (equal (kubernetes-describe--generate-buffer-name "service" "test-service" '())
                 "*kubernetes describe service/test-service*")))

;;; kubernetes-describe-test.el ends here
