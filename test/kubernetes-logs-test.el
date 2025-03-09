;;; kubernetes-logs-test.el --- Test for kubernetes logs functionality  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'kubernetes-logs)
(require 'kubernetes-utils)
(declare-function test-helper-json-resource "test-helper.el")

(defconst kubernetes-logs-test--sample-pod
  (list (cons 'metadata
              (list (cons 'name "test-pod")
                    (cons 'namespace "default")))
        (cons 'spec
              (list (cons 'containers
                          (vector (list (cons 'name "main-container"))
                                  (list (cons 'name "sidecar"))))))))

(defconst kubernetes-logs-test--sample-deployment
  (list (cons 'metadata
              (list (cons 'name "test-deployment")
                    (cons 'namespace "default")))
        (cons 'spec
              (list (cons 'template
                          (list (cons 'spec
                                      (list (cons 'containers
                                                  (vector (list (cons 'name "template-container"))))))))))))

(defconst kubernetes-logs-test--sample-job
  (list (cons 'metadata
              (list (cons 'name "test-job")
                    (cons 'namespace "default")))
        (cons 'spec
              (list (cons 'template
                          (list (cons 'spec
                                      (list (cons 'containers
                                                  (vector (list (cons 'name "job-container"))))))))))))

(defconst kubernetes-logs-test--sample-pods
  (list (cons 'items
              (vector kubernetes-logs-test--sample-pod
                      (list (cons 'metadata
                                  (list (cons 'name "deployment-pod")
                                        (cons 'namespace "default")
                                        (cons 'ownerReferences
                                              (vector (list (cons 'kind "ReplicaSet")
                                                            (cons 'name "test-deployment-12345"))))))
                            (cons 'spec
                                  (list (cons 'containers
                                              (vector (list (cons 'name "deployment-container"))
                                                      (list (cons 'name "deployment-sidecar")))))))
                      (list (cons 'metadata
                                  (list (cons 'name "job-pod")
                                        (cons 'namespace "default")
                                        (cons 'ownerReferences
                                              (vector (list (cons 'kind "Job")
                                                            (cons 'name "test-job"))))))
                            (cons 'spec
                                  (list (cons 'containers
                                              (vector (list (cons 'name "job-container-1"))
                                                      (list (cons 'name "job-sidecar")))))))))))

;; Test kubernetes-logs--read-resource-if-needed for different resource types
(ert-deftest kubernetes-logs-test--read-resource-if-needed ()
  (let ((kubernetes-overview-buffer-name "*kubernetes-test-overview*")
        (kubernetes-logs-supported-resource-types '("pod" "deployment" "statefulset" "job")))

    ;; Mock functions to avoid actual state usage
    (cl-letf (((symbol-function 'kubernetes-pods--read-name)
               (lambda (_) "fallback-pod")))

      ;; Setup overview buffer
      (let ((buf (get-buffer-create kubernetes-overview-buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (erase-buffer)

                ;; Add a pod resource (supported)
                (insert "Pod: ")
                (let ((start (point))
                      (pod-name "test-pod"))
                  (insert pod-name)
                  (let ((nav-property (list :pod-name pod-name)))
                    (put-text-property start (point) 'kubernetes-nav nav-property))
                  (insert "\n"))

                ;; Add a deployment resource (supported)
                (insert "Deployment: ")
                (let ((start (point))
                      (deployment-name "test-deployment"))
                  (insert deployment-name)
                  (let ((nav-property (list :deployment-name deployment-name)))
                    (put-text-property start (point) 'kubernetes-nav nav-property))
                  (insert "\n"))

                ;; Add a service resource (unsupported)
                (insert "Service: ")
                (let ((start (point))
                      (service-name "test-service"))
                  (insert service-name)
                  (let ((nav-property (list :service-name service-name)))
                    (put-text-property start (point) 'kubernetes-nav nav-property))
                  (insert "\n"))

                ;; Test with pod (supported resource)
                (goto-char (point-min))
                (search-forward "test-pod")
                (backward-char 3)
                (let ((result (kubernetes-logs--read-resource-if-needed nil)))
                  (should result)
                  (should (equal (car result) "pod"))
                  (should (equal (cdr result) "test-pod")))

                ;; Test with deployment (supported resource)
                (goto-char (point-min))
                (search-forward "test-deployment")
                (backward-char 3)
                (let ((result (kubernetes-logs--read-resource-if-needed nil)))
                  (should result)
                  (should (equal (car result) "deployment"))
                  (should (equal (cdr result) "test-deployment")))

                ;; Test with service (unsupported resource - should fallback to pod)
                (goto-char (point-min))
                (search-forward "test-service")
                (backward-char 3)
                (let ((result (kubernetes-logs--read-resource-if-needed nil)))
                  (should result)
                  (should (equal (car result) "pod"))
                  (should (equal (cdr result) "fallback-pod")))

                ;; Test with no resource at point (should fallback to pod)
                (goto-char (point-max))
                (let ((result (kubernetes-logs--read-resource-if-needed nil)))
                  (should result)
                  (should (equal (car result) "pod"))
                  (should (equal (cdr result) "fallback-pod")))))
          (when buf (kill-buffer buf)))))))

;; Test kubernetes-logs-fetch-all command construction
(ert-deftest kubernetes-logs-test-fetch-all-command ()
  (cl-letf (((symbol-function 'kubernetes-kubectl--flags-from-state)
             (lambda (_) '("--kubeconfig=/test/config")))
            ((symbol-function 'kubernetes-state--get)
             (lambda (_ key) (when (eq key 'current-namespace) "test-namespace")))
            ((symbol-function 'kubernetes-utils-process-buffer-start)
             (lambda (buffername _mode _exe args)
               ;; Create a real buffer and return it instead of a cons cell
               (let ((buf (get-buffer-create buffername)))
                 (with-current-buffer buf
                   (erase-buffer)
                   (insert (format "Mock command: %s" (mapconcat #'identity args " "))))
                 buf)))
            ((symbol-function 'select-window)
             (lambda (_) nil))
            ((symbol-function 'display-buffer)
             (lambda (buf) buf)))

    (unwind-protect
        (progn
          ;; Test pod logs command
          (kubernetes-logs-fetch-all "pod" "test-pod" '("--tail=10") 'mock-state)
          (with-current-buffer kubernetes-logs-buffer-name
            (should (string-match-p "logs.*--tail=10.*test-pod" (buffer-string)))
            (should (string-match-p "--namespace=test-namespace" (buffer-string))))

          ;; Test deployment logs command
          (kubernetes-logs-fetch-all "deployment" "test-deployment" '("--tail=10") 'mock-state)
          (with-current-buffer kubernetes-logs-buffer-name
            (should (string-match-p "logs.*--tail=10.*deployment/test-deployment" (buffer-string)))
            (should (string-match-p "--namespace=test-namespace" (buffer-string))))

          ;; Test job logs command
          (kubernetes-logs-fetch-all "job" "test-job" '("--timestamps=true") 'mock-state)
          (with-current-buffer kubernetes-logs-buffer-name
            (should (string-match-p "logs.*--timestamps=true.*job/test-job" (buffer-string)))
            (should (string-match-p "--namespace=test-namespace" (buffer-string)))))

      ;; Clean up test buffer
      (when (get-buffer kubernetes-logs-buffer-name)
        (kill-buffer kubernetes-logs-buffer-name)))))

;; Test kubernetes-utils--get-container-names-from-pod
(ert-deftest kubernetes-logs-test-get-container-names-from-pod ()
  (let ((pod kubernetes-logs-test--sample-pod))
    (let ((result (kubernetes-utils--get-container-names-from-pod pod)))
      (should (equal (length result) 2))
      (should (member "main-container" result))
      (should (member "sidecar" result)))))

;; Test kubernetes-utils--extract-container-names-from-spec
(ert-deftest kubernetes-logs-test-extract-container-names-from-spec ()
  (let ((pod-spec (alist-get 'spec kubernetes-logs-test--sample-pod))
        (with-init-containers
         (list (cons 'containers (vector (list (cons 'name "main"))
                                         (list (cons 'name "sidecar"))))
               (cons 'initContainers (vector (list (cons 'name "init")))))))

    ;; Test with normal pod spec
    (let ((result (kubernetes-utils--extract-container-names-from-spec pod-spec)))
      (should (equal (length result) 2))
      (should (member "main-container" result))
      (should (member "sidecar" result)))

    ;; Test with init containers
    (let ((result (kubernetes-utils--extract-container-names-from-spec with-init-containers)))
      (should (equal (length result) 3))
      (should (member "main" result))
      (should (member "sidecar" result))
      (should (member "init" result)))))

;; Test kubernetes-utils--get-all-pods-for-owner
(ert-deftest kubernetes-logs-test-get-all-pods-for-owner ()
  (let ((state (list (cons 'pods kubernetes-logs-test--sample-pods))))

    ;; Test deployment pods
    (let ((result (kubernetes-utils--get-all-pods-for-owner "deployment" "test-deployment" state)))
      (should (equal (length result) 1))
      (should (equal (alist-get 'name (alist-get 'metadata (car result))) "deployment-pod")))

    ;; Test job pods
    (let ((result (kubernetes-utils--get-all-pods-for-owner "job" "test-job" state)))
      (should (equal (length result) 1))
      (should (equal (alist-get 'name (alist-get 'metadata (car result))) "job-pod")))

    ;; Test non-existent owner
    (should (equal (kubernetes-utils--get-all-pods-for-owner "deployment" "non-existent" state) nil))))

;; Test kubernetes-utils--get-container-names with mocked internals
(ert-deftest kubernetes-logs-test-get-container-names ()
  (let ((mock-state (list (cons 'pods kubernetes-logs-test--sample-pods))))
    (cl-letf (((symbol-function 'kubernetes-state-lookup-pod)
               (lambda (name _)
                 (when (equal name "test-pod")
                   kubernetes-logs-test--sample-pod)))
              ((symbol-function 'kubernetes-state-lookup-deployment)
               (lambda (name _)
                 (when (equal name "test-deployment")
                   kubernetes-logs-test--sample-deployment)))
              ((symbol-function 'kubernetes-state-lookup-job)
               (lambda (name _)
                 (when (equal name "test-job")
                   kubernetes-logs-test--sample-job))))

      ;; Test pod containers
      (let ((result (kubernetes-utils--get-container-names "pod" "test-pod" mock-state)))
        (should (equal (length result) 2))
        (should (member "main-container" result))
        (should (member "sidecar" result)))

      ;; Test deployment containers (should get them from found pods)
      (let ((result (kubernetes-utils--get-container-names "deployment" "test-deployment" mock-state)))
        (should (equal (length result) 2))
        (should (member "deployment-container" result))
        (should (member "deployment-sidecar" result)))

      ;; Test job containers (should get them from found pods)
      (let ((result (kubernetes-utils--get-container-names "job" "test-job" mock-state)))
        (should (equal (length result) 2))
        (should (member "job-container-1" result))
        (should (member "job-sidecar" result))))))

;; Test kubernetes-utils-read-container-name
(ert-deftest kubernetes-logs-test-read-container-name ()
  (let ((kubernetes-overview-buffer-name "*kubernetes-test-overview*")
        (kubernetes-logs-supported-resource-types '("pod" "deployment" "statefulset" "job")))

    ;; Mock functions for state and container lookup
    (cl-letf (((symbol-function 'kubernetes-state)
               (lambda () 'mock-state))
              ((symbol-function 'kubernetes-pods--read-name)
               (lambda (_) "test-pod"))
              ((symbol-function 'kubernetes-state-lookup-pod)
               (lambda (name _) kubernetes-logs-test--sample-pod))
              ((symbol-function 'kubernetes-utils--get-container-names)
               (lambda (type name _)
                 (cond
                  ((and (equal type "pod") (equal name "test-pod"))
                   '("main-container" "sidecar"))
                  ((and (equal type "deployment") (equal name "test-deployment"))
                   '("deployment-container" "deployment-sidecar"))
                  (t nil))))
              ((symbol-function 'kubernetes-utils--get-container-names-from-pod)
               (lambda (_) '("main-container" "sidecar")))
              ((symbol-function 'completing-read)
               (lambda (prompt collection &rest _) (car collection))))

      ;; Set up overview buffer
      (let ((buf (get-buffer-create kubernetes-overview-buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (erase-buffer)

                ;; Add a pod resource
                (insert "Pod: ")
                (let ((start (point))
                      (pod-name "test-pod"))
                  (insert pod-name)
                  (let ((nav-property (list :pod-name pod-name)))
                    (put-text-property start (point) 'kubernetes-nav nav-property))
                  (insert "\n"))

                ;; Add a deployment resource
                (insert "Deployment: ")
                (let ((start (point))
                      (deployment-name "test-deployment"))
                  (insert deployment-name)
                  (let ((nav-property (list :deployment-name deployment-name)))
                    (put-text-property start (point) 'kubernetes-nav nav-property))
                  (insert "\n"))

                ;; Add a service resource (unsupported)
                (insert "Service: ")
                (let ((start (point))
                      (service-name "test-service"))
                  (insert service-name)
                  (let ((nav-property (list :service-name service-name)))
                    (put-text-property start (point) 'kubernetes-nav nav-property))
                  (insert "\n"))

                ;; Test with pod (supported)
                (goto-char (point-min))
                (search-forward "test-pod")
                (backward-char 3)
                (let ((result (kubernetes-utils-read-container-name "Select container: ")))
                  (should (equal result "main-container")))

                ;; Test with deployment (supported)
                (goto-char (point-min))
                (search-forward "test-deployment")
                (backward-char 3)
                (let ((result (kubernetes-utils-read-container-name "Select container: ")))
                  (should (equal result "deployment-container")))

                ;; Test with service (unsupported - should fall back to pod)
                (goto-char (point-min))
                (search-forward "test-service")
                (backward-char 3)
                (let ((result (kubernetes-utils-read-container-name "Select container: ")))
                  (should (equal result "main-container")))))
          (when buf (kill-buffer buf)))))))

;;; kubernetes-logs-test.el ends here
