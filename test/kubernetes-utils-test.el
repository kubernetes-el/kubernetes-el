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
    (should-error (kubernetes-utils-read-container-name "Containers: ") :type 'kubernetes-state-error)))

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

;; Tests for kubernetes-logs-supported-resource-types and kubernetes-logs--read-resource-if-needed

(ert-deftest kubernetes-utils-test-logs-supported-resource-types ()
  "Test that kubernetes-logs-supported-resource-types contains the expected values."
  (should (equal kubernetes-logs-supported-resource-types '("pod" "deployment" "statefulset" "job" "service"))))

(ert-deftest kubernetes-utils-test-logs-read-resource-if-needed ()
  "Test `kubernetes-logs--read-resource-if-needed' behavior."
  (let ((kubernetes-overview-buffer-name "*kubernetes-test-overview*"))

    ;; Mock functions to avoid actual state usage
    (cl-letf (((symbol-function 'kubernetes-pods--read-name)
               (lambda (_) "fallback-pod")))

      (unwind-protect
          (progn
            (kubernetes-utils-test-setup-overview-buffer)
            (with-current-buffer kubernetes-overview-buffer-name
              (erase-buffer)

              ;; Add a pod resource (supported)
              (insert "Pod: ")
              (let ((start (point))
                    (pod-name "nginx-pod"))
                (insert pod-name)
                (kubernetes-utils-test-add-nav-property "pod" pod-name start (point))
                (insert "\n"))

              ;; Add a ingress resource (unsupported)
              (insert "Ingress: ")
              (let ((start (point))
                    (ingress-name "frontend-ingress"))
                (insert ingress-name)
                (kubernetes-utils-test-add-nav-property "ingress" ingress-name start (point)))

              ;; Test with supported resource
              (goto-char (point-min))
              (search-forward "nginx-pod")
              (backward-char 3)
              (let ((result (kubernetes-logs--read-resource-if-needed nil)))
                (should result)
                (should (equal (car result) "pod"))
                (should (equal (cdr result) "nginx-pod")))

              ;; Test with unsupported resource (should fall back to pod selection)
              (goto-char (point-min))
              (search-forward "frontend-ingress")
              (backward-char 3)
              (let ((result (kubernetes-logs--read-resource-if-needed nil)))
                (should result)
                (should (equal (car result) "pod"))
                (should (equal (cdr result) "fallback-pod")))

              ;; Test with no resource at point (should fall back to pod selection)
              (goto-char (point-min))
              (let ((result (kubernetes-logs--read-resource-if-needed nil)))
                (should result)
                (should (equal (car result) "pod"))
                (should (equal (cdr result) "fallback-pod")))))
        (kubernetes-utils-test-teardown-overview-buffer)))))

(ert-deftest kubernetes-utils-test-logs-follow-and-fetch-all ()
  "Test the refactored `kubernetes-logs-follow' and `kubernetes-logs-fetch-all' functions."
  (let ((kubernetes-overview-buffer-name "*kubernetes-test-overview*")
        (test-buffers '())
        (expected-buffer-patterns '()))

    ;; Mock functions to avoid actual kubectl execution and buffer creation
    (cl-letf (((symbol-function 'kubernetes-pods--read-name)
               (lambda (_) "fallback-pod"))
              ((symbol-function 'kubernetes-kubectl--flags-from-state)
               (lambda (_) '("--kubeconfig=/mock/path")))
              ((symbol-function 'kubernetes-state--get)
               (lambda (_ key) (when (eq key 'current-namespace) "test-namespace")))
              ((symbol-function 'kubernetes-utils-process-buffer-start)
               (lambda (buffername _mode _exe args)
                 ;; Save the buffer name pattern we expect
                 (push buffername expected-buffer-patterns)
                 ;; Create and return a buffer
                 (let ((buf (get-buffer-create buffername)))
                   (push buf test-buffers)
                   (with-current-buffer buf
                     (erase-buffer)
                     (insert (format "Mock process with args: %s" (mapconcat #'identity args " "))))
                   buf)))
              ((symbol-function 'select-window)
               (lambda (_) nil))
              ((symbol-function 'display-buffer)
               (lambda (buf) buf)))

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

              ;; Test kubernetes-logs-follow with pod - just call it, don't check return value
              (goto-char (point-min))
              (search-forward "nginx-pod")
              (backward-char 3)
              (kubernetes-logs-follow '("--tail=10") 'mock-state)

              ;; Instead check if a buffer with the expected name pattern exists
              (let ((pod-pattern "*kubernetes logs: test-namespace/pod/nginx-pod*"))
                (should (member pod-pattern expected-buffer-patterns))

                ;; Find the buffer with our pattern
                (let ((matching-buffer (seq-find
                                        (lambda (buf)
                                          (string= (buffer-name buf) pod-pattern))
                                        test-buffers)))
                  (should matching-buffer)
                  (when matching-buffer
                    (with-current-buffer matching-buffer
                      (should (string-match-p "logs.*-f.*--tail=10.*nginx-pod" (buffer-string)))
                      (should (string-match-p "--namespace=test-namespace" (buffer-string)))))))

              ;; Test kubernetes-logs-follow with deployment
              (goto-char (point-min))
              (search-forward "web-deployment")
              (backward-char 3)
              (kubernetes-logs-follow '("--tail=10") 'mock-state)

              ;; Check for deployment buffer
              (let ((deployment-pattern "*kubernetes logs: test-namespace/deployment/web-deployment*"))
                (should (member deployment-pattern expected-buffer-patterns))

                ;; Find the buffer with our pattern
                (let ((matching-buffer (seq-find
                                        (lambda (buf)
                                          (string= (buffer-name buf) deployment-pattern))
                                        test-buffers)))
                  (should matching-buffer)
                  (when matching-buffer
                    (with-current-buffer matching-buffer
                      (should (string-match-p "logs.*-f.*--tail=10.*deployment/web-deployment" (buffer-string)))
                      (should (string-match-p "--namespace=test-namespace" (buffer-string)))))))

              ;; Test direct call to kubernetes-logs-fetch-all
              (kubernetes-logs-fetch-all "statefulset" "db-statefulset" '("--timestamps=true") 'mock-state)

              ;; Check for statefulset buffer
              (let ((statefulset-pattern "*kubernetes logs: test-namespace/statefulset/db-statefulset*"))
                (should (member statefulset-pattern expected-buffer-patterns))

                ;; Find the buffer with our pattern
                (let ((matching-buffer (seq-find
                                        (lambda (buf)
                                          (string= (buffer-name buf) statefulset-pattern))
                                        test-buffers)))
                  (should matching-buffer)
                  (when matching-buffer
                    (with-current-buffer matching-buffer
                      (should (string-match-p "logs.*--timestamps=true.*statefulset/db-statefulset" (buffer-string)))
                      (should (string-match-p "--namespace=test-namespace" (buffer-string)))))))))

        ;; Clean up all test buffers
        (dolist (buf test-buffers)
          (when (buffer-live-p buf)
            (kill-buffer buf)))
        (kubernetes-utils-test-teardown-overview-buffer)))))

;; Test for kubernetes-utils-read-container-name with the new logs module implementation
(ert-deftest kubernetes-utils-test-read-container-name-with-logs-module ()
  "Test `kubernetes-utils-read-container-name' with the new logs module."
  (let ((kubernetes-overview-buffer-name "*kubernetes-test-overview*"))

    ;; Mock functions to avoid actual state and pod lookup
    (cl-letf (((symbol-function 'kubernetes-state)
               (lambda () 'mock-state))
              ((symbol-function 'kubernetes-pods--read-name)
               (lambda (_) "selected-pod"))
              ((symbol-function 'kubernetes-state-lookup-pod)
               (lambda (name _) (list :test-pod name)))
              ((symbol-function 'kubernetes-utils--get-container-names)
               (lambda (type name _)
                 (when (and (member type kubernetes-logs-supported-resource-types)
                            (or (equal name "nginx-pod")
                                (equal name "web-deployment")))
                   '("container1" "container2"))))
              ((symbol-function 'kubernetes-utils--get-container-names-from-pod)
               (lambda (_) '("podcontainer1" "podcontainer2")))
              ((symbol-function 'completing-read)
               (lambda (prompt collection &rest _)
                 (concat prompt (car collection)))))

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
                (kubernetes-utils-test-add-nav-property "deployment" deployment-name start (point))
                (insert "\n"))

              ;; Add a ingress resource (unsupported)
              (insert "Ingress: ")
              (let ((start (point))
                    (ingress-name "frontend-ingress"))
                (insert ingress-name)
                (kubernetes-utils-test-add-nav-property "ingress" ingress-name start (point)))

              ;; Test with pod (supported)
              (goto-char (point-min))
              (search-forward "nginx-pod")
              (backward-char 3)
              (should (equal (kubernetes-utils-read-container-name "Test prompt: ")
                             "Test prompt: container1"))

              ;; Test with deployment (supported)
              (goto-char (point-min))
              (search-forward "web-deployment")
              (backward-char 3)
              (should (equal (kubernetes-utils-read-container-name "Test prompt: ")
                             "Test prompt: container1"))

              ;; Test with ingress (unsupported, should fall back to pod selection)
              (goto-char (point-min))
              (search-forward "frontend-ingress")
              (backward-char 3)
              (should (equal (kubernetes-utils-read-container-name "Test prompt: ")
                             "Test prompt: podcontainer1"))))
        (kubernetes-utils-test-teardown-overview-buffer)))))

;; Tests for the resource-specific maybe-*-name-at-point functions
(ert-deftest kubernetes-utils-test-maybe-deployment-name-at-point ()
  "Test `kubernetes-utils-maybe-deployment-name-at-point'."
  (let ((kubernetes-overview-buffer-name "*kubernetes-test-overview*"))
    (unwind-protect
        (progn
          (kubernetes-utils-test-setup-overview-buffer)
          (with-current-buffer kubernetes-overview-buffer-name
            (erase-buffer)

            ;; Add resource
            (insert "Deployment: ")
            (let ((start (point))
                  (deployment-name "web-deployment"))
              (insert deployment-name)
              (kubernetes-utils-test-add-nav-property "deployment" deployment-name start (point)))

            ;; Test deployment detection
            (goto-char (point-min))
            (search-forward "web-deployment")
            (backward-char 3)
            (should (equal (kubernetes-utils-maybe-deployment-name-at-point) "web-deployment"))))
      (kubernetes-utils-test-teardown-overview-buffer))))

(ert-deftest kubernetes-utils-test-maybe-statefulset-name-at-point ()
  "Test `kubernetes-utils-maybe-statefulset-name-at-point'."
  (let ((kubernetes-overview-buffer-name "*kubernetes-test-overview*"))
    (unwind-protect
        (progn
          (kubernetes-utils-test-setup-overview-buffer)
          (with-current-buffer kubernetes-overview-buffer-name
            (erase-buffer)

            ;; Add resource
            (insert "StatefulSet: ")
            (let ((start (point))
                  (statefulset-name "db-statefulset"))
              (insert statefulset-name)
              (kubernetes-utils-test-add-nav-property "statefulset" statefulset-name start (point)))

            ;; Test detection
            (goto-char (point-min))
            (search-forward "db-statefulset")
            (backward-char 3)
            (should (equal (kubernetes-utils-maybe-statefulset-name-at-point) "db-statefulset"))))
      (kubernetes-utils-test-teardown-overview-buffer))))

(ert-deftest kubernetes-utils-test-maybe-job-name-at-point ()
  "Test `kubernetes-utils-maybe-job-name-at-point'."
  (let ((kubernetes-overview-buffer-name "*kubernetes-test-overview*"))
    (unwind-protect
        (progn
          (kubernetes-utils-test-setup-overview-buffer)
          (with-current-buffer kubernetes-overview-buffer-name
            (erase-buffer)

            ;; Add resource
            (insert "Job: ")
            (let ((start (point))
                  (job-name "backup-job"))
              (insert job-name)
              (kubernetes-utils-test-add-nav-property "job" job-name start (point)))

            ;; Test detection
            (goto-char (point-min))
            (search-forward "backup-job")
            (backward-char 3)
            (should (equal (kubernetes-utils-maybe-job-name-at-point) "backup-job"))))
      (kubernetes-utils-test-teardown-overview-buffer))))

;; Test for extract-container-names-from-spec
(ert-deftest kubernetes-utils-test-extract-container-names-from-spec ()
  "Test `kubernetes-utils--extract-container-names-from-spec'."
  (let ((pod-spec '((containers . [((name . "main-container"))
                                  ((name . "sidecar-container"))])
                    (initContainers . [((name . "init-container"))]))))
    (let ((result (kubernetes-utils--extract-container-names-from-spec pod-spec)))
      (should (equal (length result) 3))
      (should (member "main-container" result))
      (should (member "sidecar-container" result))
      (should (member "init-container" result)))))

;; Test for get-container-names-from-pod
(ert-deftest kubernetes-utils-test-get-container-names-from-pod ()
  "Test `kubernetes-utils--get-container-names-from-pod'."
  (let ((pod '((spec . ((containers . [((name . "main-container"))
                                       ((name . "sidecar-container"))])
                        (initContainers . [((name . "init-container"))]))))))
    (let ((result (kubernetes-utils--get-container-names-from-pod pod)))
      (should (equal (length result) 3))
      (should (member "main-container" result))
      (should (member "sidecar-container" result))
      (should (member "init-container" result)))))

(ert-deftest kubernetes-utils--extract-container-name-from-args-test ()
  "Test `kubernetes-utils--extract-container-name-from-args` function."
  (should (equal (kubernetes-utils--extract-container-name-from-args '("--container=my-container"))
                 "my-container"))
  (should (equal (kubernetes-utils--extract-container-name-from-args '("--some-other-arg" "--another-arg"))
                 nil))
  (should (equal (kubernetes-utils--extract-container-name-from-args nil)
                 nil)))

(ert-deftest kubernetes-utils-test-get-resource-name ()
  "Test ~kubernetes-utils-get-resource-name~ function."
  (let ((state '((current-namespace . "default"))))

    ;; 1. Test with a resource name available via a specific function
    (cl-letf (((symbol-function 'kubernetes-utils-maybe-pod-name-at-point)
               (lambda () "pod-at-point")))
      (let ((result (kubernetes-utils-get-resource-name state "pod")))
        (should (equal result "pod-at-point"))))

    ;; 2. Test with a resource-specific read function
    (cl-letf (((symbol-function 'kubernetes-pods--read-name)
               (lambda (_state) "read-pod-name")))
      (let ((result (kubernetes-utils-get-resource-name state "pod")))
        (should (equal result "read-pod-name"))))

    ;; 3. Test with a dynamically constructed function for ~persistentvolumeclaim~
    (cl-letf (((symbol-function 'kubernetes-utils-maybe-pvc-name-at-point)
               (lambda () "pvc-at-point")))
      (let ((result (kubernetes-utils-get-resource-name state "persistentvolumeclaim")))
        (should (equal result "pvc-at-point"))))

    ;; 4. Test with a resource-specific read function for ~persistentvolumeclaim~
    (cl-letf (((symbol-function 'kubernetes-persistentvolumeclaims--read-name)
               (lambda (_state) "read-pvc-name")))
      (let ((result (kubernetes-utils-get-resource-name state "persistentvolumeclaim")))
        (should (equal result "read-pvc-name"))))

    ;; 5. Test with no specific function or read function available
    (cl-letf (((symbol-function 'kubernetes-utils-maybe-service-name-at-point)
               (lambda () nil))
              ((symbol-function 'kubernetes-services--read-name)
               (lambda (_state) nil)))
      (let ((result (kubernetes-utils-get-resource-name state "service")))
        (should (equal result nil))))))

(ert-deftest kubernetes-utils-test-select-resource ()
  "Test `kubernetes-utils-select-resource' function."
  (let ((state '((current-namespace . "default"))))
    ;; Mock the interactive functions
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt choices &rest _)
                 (should (string= prompt "Resource type: "))
                 (should (member "pod" choices))
                 (should (member "deployment" choices))
                 "deployment"))
              ((symbol-function 'kubernetes-utils-get-resource-name)
               (lambda (test-state resource-type)
                 (should (equal test-state state))
                 (should (equal resource-type "deployment"))
                 "web-deployment")))

      ;; Call the function and check results
      (let ((result (kubernetes-utils-select-resource
                     state
                     '("pod" "deployment" "statefulset" "job"))))
        (should (equal result '("deployment" . "web-deployment")))))))

;;; kubernetes-utils-test.el ends here
