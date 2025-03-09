;;; kubernetes-utils-test.el --- Test util functions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(require 'kubernetes-overview)
(require 'kubernetes-utils)
(declare-function test-helper-json-resource "test-helper.el")

(ert-deftest kubernetes-utils-test--get-pod-container-names--invalid-input ()
  (should (equal nil (kubernetes-get-pod-container-names '()))))

(ert-deftest kubernetes-utils-test--get-pod-container-names ()
  (-let* ((res (test-helper-json-resource "get-pods-response.json"))
          ((&alist 'items [pod]) res))
    (should (equal '("example-service-1") (kubernetes-get-pod-container-names pod)))))

(ert-deftest kubernetes-utils-test--maybe-pod-name-at-point--not-in-overview-buffer ()
  (ignore-errors
    (kill-buffer kubernetes-overview-buffer-name))
  (should-not (kubernetes-utils-maybe-pod-name-at-point)))

(ert-deftest kubernetes-utils-test--read-container-name--no-state ()
  (cl-letf (((symbol-function 'kubernetes-state) #'ignore))
    (should-error (kubernetes-utils-read-container-name) :type 'kubernetes-state-error)))

(defmacro kubernetes-utils-test-with-temp-buffer (initial-contents &rest body)
  "Create temp buffer with INITIAL-CONTENTS and execute BODY.
Point is moved to the position indicated by | in INITIAL-CONTENTS."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,initial-contents)
     (goto-char (point-min))
     (when (search-forward "|" nil t)
       (delete-char -1))
     ,@body))

(defun kubernetes-utils-test-add-nav-property (resource-type resource-name start end)
  "Add kubernetes-nav property for RESOURCE-TYPE and RESOURCE-NAME from START to END."
  (let ((nav-property (list (intern (format ":%s-name" resource-type)) resource-name)))
    (put-text-property start end 'kubernetes-nav nav-property)))

(defun kubernetes-utils-test-setup-overview-buffer ()
  "Set up a mock kubernetes overview buffer for testing."
  (let ((buf (get-buffer-create kubernetes-overview-buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (insert "Test kubernetes buffer content."))
    buf))

(defun kubernetes-utils-test-teardown-overview-buffer ()
  "Clean up the mock kubernetes overview buffer after testing."
  (when-let ((buf (get-buffer kubernetes-overview-buffer-name)))
    (kill-buffer buf)))

(ert-deftest kubernetes-utils-test-get-resource-info-at-point-nil ()
  "Test `kubernetes-utils-get-resource-info-at-point' returns nil when no resource is at point."
  (let ((kubernetes-overview-buffer-name "*kubernetes-test-overview*"))
    (unwind-protect
        (progn
          (kubernetes-utils-test-setup-overview-buffer)
          (with-current-buffer kubernetes-overview-buffer-name
            (erase-buffer)
            (insert "No kubernetes resource properties here.")
            (goto-char (point-min))
            (should (null (kubernetes-utils-get-resource-info-at-point)))))
      (kubernetes-utils-test-teardown-overview-buffer))))

;; Generate test for each resource type
(let ((resource-types '(("pod" . "nginx-pod")
                        ("deployment" . "web-deployment")
                        ("service" . "frontend-service")
                        ("configmap" . "app-config")
                        ("secret" . "app-secrets")
                        ("ingress" . "app-ingress")
                        ("job" . "backup-job")
                        ("cronjob" . "cleanup-cronjob")
                        ("statefulset" . "db-statefulset")
                        ("persistentvolumeclaim" . "data-pvc")
                        ("networkpolicy" . "allow-internal")
                        ("node" . "worker-node-1"))))

  (dolist (resource-type-pair resource-types)
    (let ((resource-type (car resource-type-pair))
          (resource-name (cdr resource-type-pair)))
      (eval
       `(ert-deftest ,(intern (format "kubernetes-utils-test-get-resource-info-at-point-%s" resource-type)) ()
          ,(format "Test `kubernetes-utils-get-resource-info-at-point' with %s resource." resource-type)
          (let ((kubernetes-overview-buffer-name "*kubernetes-test-overview*"))
            (unwind-protect
                (progn
                  (kubernetes-utils-test-setup-overview-buffer)
                  (with-current-buffer kubernetes-overview-buffer-name
                    (erase-buffer)
                    (insert ,(format "Test %s: " resource-type))
                    (let ((start (point))
                          (name ,resource-name))
                      (insert name)
                      (kubernetes-utils-test-add-nav-property ,resource-type name start (point))

                      ;; Test at the beginning of name
                      (goto-char start)
                      (let ((result (kubernetes-utils-get-resource-info-at-point)))
                        (should result)
                        (should (equal (car result) ,resource-type))
                        (should (equal (cdr result) ,resource-name)))

                      ;; Test at the middle of name
                      (goto-char (+ start (/ (length name) 2)))
                      (let ((result (kubernetes-utils-get-resource-info-at-point)))
                        (should result)
                        (should (equal (car result) ,resource-type))
                        (should (equal (cdr result) ,resource-name)))

                      ;; Test right after the text with property
                      (goto-char (+ start (length name) 1))
                      (insert " ") ;; Ensure there's space after the property
                      (let ((result (kubernetes-utils-get-resource-info-at-point)))
                        (should (null result))))))
              (kubernetes-utils-test-teardown-overview-buffer))))))))

;; Test resource name extraction function
(ert-deftest kubernetes-utils-test-get-resource-name-at-point ()
  "Test `kubernetes-utils-get-resource-name-at-point'."
  (let ((kubernetes-overview-buffer-name "*kubernetes-test-overview*"))
    (unwind-protect
        (progn
          (kubernetes-utils-test-setup-overview-buffer)
          (with-current-buffer kubernetes-overview-buffer-name
            (erase-buffer)

            ;; Add a pod resource
            (insert "Pod: ")
            (let ((start (point))
                  (pod-name "nginx-pod"))
              (insert pod-name)
              (kubernetes-utils-test-add-nav-property "pod" pod-name start (point))
              (insert "\n"))

            ;; Add a deployment resource
            (insert "Deployment: ")
            (let ((start (point))
                  (deployment-name "web-deployment"))
              (insert deployment-name)
              (kubernetes-utils-test-add-nav-property "deployment" deployment-name start (point)))

            ;; Test with specific type
            (goto-char (point-min))
            (search-forward "nginx-pod")
            (backward-char 3)
            (should (equal (kubernetes-utils-get-resource-name-at-point "pod") "nginx-pod"))
            (should (null (kubernetes-utils-get-resource-name-at-point "deployment")))

            ;; Test with another type
            (goto-char (point-min))
            (search-forward "web-deployment")
            (backward-char 3)
            (should (equal (kubernetes-utils-get-resource-name-at-point "deployment") "web-deployment"))
            (should (null (kubernetes-utils-get-resource-name-at-point "pod")))

            ;; Test without type parameter (should return any resource name)
            (goto-char (point-min))
            (search-forward "nginx-pod")
            (backward-char 3)
            (should (equal (kubernetes-utils-get-resource-name-at-point) "nginx-pod"))

            (goto-char (point-min))
            (search-forward "web-deployment")
            (backward-char 3)
            (should (equal (kubernetes-utils-get-resource-name-at-point) "web-deployment"))))
      (kubernetes-utils-test-teardown-overview-buffer))))

;; Test that complex property names still work correctly
(ert-deftest kubernetes-utils-test-get-resource-info-complex-names ()
  "Test `kubernetes-utils-get-resource-info-at-point' with complex resource names."
  (let ((kubernetes-overview-buffer-name "*kubernetes-test-overview*"))
    (unwind-protect
        (progn
          (kubernetes-utils-test-setup-overview-buffer)
          (with-current-buffer kubernetes-overview-buffer-name
            (erase-buffer)

            ;; Hyphens in resource name
            (insert "Pod with hyphens: ")
            (let ((start (point))
                  (pod-name "nginx-frontend-v1"))
              (insert pod-name)
              (kubernetes-utils-test-add-nav-property "pod" pod-name start (point))
              (insert "\n"))

            ;; Special characters in resource name
            (insert "Pod with special chars: ")
            (let ((start (point))
                  (pod-name "app.v1.2_test"))
              (insert pod-name)
              (kubernetes-utils-test-add-nav-property "pod" pod-name start (point)))

            ;; Test with hyphenated name
            (goto-char (point-min))
            (search-forward "nginx-frontend-v1")
            (backward-char 3)
            (let ((result (kubernetes-utils-get-resource-info-at-point)))
              (should result)
              (should (equal (car result) "pod"))
              (should (equal (cdr result) "nginx-frontend-v1")))

            ;; Test with special character name
            (goto-char (point-min))
            (search-forward "app.v1.2_test")
            (backward-char 3)
            (let ((result (kubernetes-utils-get-resource-info-at-point)))
              (should result)
              (should (equal (car result) "pod"))
              (should (equal (cdr result) "app.v1.2_test")))))
      (kubernetes-utils-test-teardown-overview-buffer))))

(ert-deftest kubernetes-utils-test-get-resource-info-nonstandard-property ()
  "Test handling of non-standard kubernetes-nav properties."
  (let ((kubernetes-overview-buffer-name "*kubernetes-test-overview*"))
    (unwind-protect
        (progn
          (kubernetes-utils-test-setup-overview-buffer)
          (with-current-buffer kubernetes-overview-buffer-name
            (erase-buffer)

            ;; Add a property that doesn't follow the :type-name pattern
            (insert "Strange property: ")
            (let ((start (point)))
              (insert "something")
              (let ((nav-property (list :not-a-resource-type "something")))
                (put-text-property start (point) 'kubernetes-nav nav-property))
              (insert "\n"))

            ;; Add a property that has extra parts in the name
            (insert "Extra parts: ")
            (let ((start (point)))
              (insert "resource-name")
              (let ((nav-property (list :some-extended-resource-type-name "resource-name")))
                (put-text-property start (point) 'kubernetes-nav nav-property)))

            ;; Test with non-standard property
            (goto-char (point-min))
            (search-forward "something")
            (backward-char 3)
            ;; Should extract the type according to our splitting logic
            (let ((result (kubernetes-utils-get-resource-info-at-point)))
              (should result)
              ;; The actual result from our implementation would be "not-a-resource-type"
              ;; because we split on "-name" and this property doesn't have that suffix
              (should (equal (car result) "not-a-resource-type"))
              (should (equal (cdr result) "something")))

            ;; Test with property that has extra parts
            (goto-char (point-min))
            (search-forward "resource-name")
            (backward-char 3)
            (let ((result (kubernetes-utils-get-resource-info-at-point)))
              (should result)
              ;; Should extract "some-extended-resource-type" as the type
              (should (equal (car result) "some-extended-resource-type"))
              (should (equal (cdr result) "resource-name")))))
      (kubernetes-utils-test-teardown-overview-buffer))))

;;; kubernetes-utils-test.el ends here
