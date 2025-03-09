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

;; Test kubernetes-logs--generate-buffer-name function
(ert-deftest kubernetes-logs-test-generate-buffer-name ()
  (cl-letf (((symbol-function 'kubernetes-state--get)
             (lambda (state key)
               (when (eq key 'current-namespace)
                 "test-namespace"))))

    ;; Test basic pod without container
    (let ((args '())
          (state '()))
      (should (equal (kubernetes-logs--generate-buffer-name "pod" "nginx" args state)
                    "*kubernetes logs: test-namespace/pod/nginx*")))

    ;; Test pod with container
    (let ((args '("--container=app"))
          (state '()))
      (should (equal (kubernetes-logs--generate-buffer-name "pod" "nginx" args state)
                    "*kubernetes logs: test-namespace/pod/nginx:app*")))

    ;; Test deployment
    (let ((args '())
          (state '()))
      (should (equal (kubernetes-logs--generate-buffer-name "deployment" "frontend" args state)
                    "*kubernetes logs: test-namespace/deployment/frontend*")))

    ;; Test deployment with container
    (let ((args '("--container=web"))
          (state '()))
      (should (equal (kubernetes-logs--generate-buffer-name "deployment" "frontend" args state)
                    "*kubernetes logs: test-namespace/deployment/frontend:web*")))))


(ert-deftest kubernetes-logs-test-fetch-all-command ()
  "Test that kubectl commands are constructed correctly for different resources."
  (let ((commands-executed nil))
    (cl-letf (((symbol-function 'kubernetes-kubectl--flags-from-state)
               (lambda (_) '("--kubeconfig=/test/config")))
              ((symbol-function 'kubernetes-state--get)
               (lambda (_ key) (when (eq key 'current-namespace) "test-namespace")))
              ((symbol-function 'kubernetes-utils-process-buffer-start)
               (lambda (buffer-name mode executable args)
                 ;; Store the command for verification
                 (push (list buffer-name executable args) commands-executed)
                 ;; Return a dummy buffer
                 (generate-new-buffer "*test-buffer*")))
              ((symbol-function 'kubernetes-logs--generate-buffer-name)
               (lambda (resource-type resource-name args state)
                 (format "*kubernetes logs: %s/%s/%s%s*"
                         (or (kubernetes-state--get state 'current-namespace) "default")
                         resource-type
                         resource-name
                         (let ((container-arg (seq-find (lambda (arg) (string-prefix-p "--container=" arg)) args)))
                           (if container-arg
                               (format ":%s" (substring container-arg (length "--container=")))
                             "")))))
              ((symbol-function 'select-window) #'ignore)
              ((symbol-function 'display-buffer) #'identity))

      (unwind-protect
          (progn
            ;; Test pod logs command
            (kubernetes-logs-fetch-all "pod" "test-pod" '("--tail=10") 'mock-state)

            ;; Test pod logs with container
            (kubernetes-logs-fetch-all "pod" "test-pod" '("--container=main-container") 'mock-state)

            ;; Test deployment logs command
            (kubernetes-logs-fetch-all "deployment" "test-deployment" '("--tail=10") 'mock-state)

            ;; Test job logs command
            (kubernetes-logs-fetch-all "job" "test-job" '("--timestamps=true") 'mock-state)

            ;; Now verify all the commands (commands are pushed in reverse order)
            (let ((job-cmd (pop commands-executed))
                  (deployment-cmd (pop commands-executed))
                  (pod-container-cmd (pop commands-executed))
                  (pod-cmd (pop commands-executed)))

              ;; Verify pod command
              (should (string-match-p "\\*kubernetes logs: test-namespace/pod/test-pod\\*" (nth 0 pod-cmd)))
              (should (equal (nth 1 pod-cmd) kubernetes-kubectl-executable))
              (let ((args (nth 2 pod-cmd)))
                (should (equal (nth 0 args) "logs"))
                (should (member "--tail=10" args))
                (should (member "test-pod" args))
                (should (member "--namespace=test-namespace" args)))

              ;; Verify pod with container command
              (should (string-match-p "\\*kubernetes logs: test-namespace/pod/test-pod:main-container\\*" (nth 0 pod-container-cmd)))
              (let ((args (nth 2 pod-container-cmd)))
                (should (equal (nth 0 args) "logs"))
                (should (member "--container=main-container" args))
                (should (member "test-pod" args))
                (should (member "--namespace=test-namespace" args)))

              ;; Verify deployment command
              (should (string-match-p "\\*kubernetes logs: test-namespace/deployment/test-deployment\\*" (nth 0 deployment-cmd)))
              (let ((args (nth 2 deployment-cmd)))
                (should (equal (nth 0 args) "logs"))
                (should (member "--tail=10" args))
                (should (member "deployment/test-deployment" args))
                (should (member "--namespace=test-namespace" args)))

              ;; Verify job command
              (should (string-match-p "\\*kubernetes logs: test-namespace/job/test-job\\*" (nth 0 job-cmd)))
              (let ((args (nth 2 job-cmd)))
                (should (equal (nth 0 args) "logs"))
                (should (member "--timestamps=true" args))
                (should (member "job/test-job" args))
                (should (member "--namespace=test-namespace" args)))))

        ;; Clean up any temp buffers
        (let ((test-buffer (get-buffer "*test-buffer*")))
          (when (buffer-live-p test-buffer)
            (kill-buffer test-buffer)))))))

;; Test kubernetes-logs-refresh functionality
(ert-deftest kubernetes-logs-test-refresh ()
  (let ((test-buffer (generate-new-buffer "*kubernetes logs: test-namespace/pod/nginx*")))
    (unwind-protect
        (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                  ((symbol-function 'kubernetes-utils-process-buffer-start)
                   (lambda (buffer-name mode executable args)
                     (should (equal buffer-name "*kubernetes logs: test-namespace/pod/nginx*"))
                     (should (equal mode #'kubernetes-logs-mode))
                     (should (equal executable kubernetes-kubectl-executable))
                     (should (equal args '("logs" "nginx" "--context=minikube" "--namespace=test-namespace")))
                     test-buffer)))
          ;; Set up the buffer with stored kubectl command args
          (with-current-buffer test-buffer
            (setq-local kubernetes-logs-resource-type "pod")
            (setq-local kubernetes-logs-resource-name "nginx")
            (setq-local kubernetes-logs-namespace "test-namespace")
            (setq-local kubernetes-logs-kubectl-args
                        '("logs" "nginx" "--context=minikube" "--namespace=test-namespace"))
            (kubernetes-logs-refresh)))
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer)))))

;; Test kubernetes-logs-refresh error handling when args are missing
(ert-deftest kubernetes-logs-test-refresh-missing-args ()
  (let ((message-log nil)
        (test-buffer (generate-new-buffer "*kubernetes logs: test-namespace/pod/nginx*")))
    (unwind-protect
        (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (setq message-log (apply #'format format-string args)))))
          (with-current-buffer test-buffer
            ;; Don't set kubernetes-logs-kubectl-args
            (kubernetes-logs-refresh)
            (should (equal message-log "Cannot refresh logs: command information not available"))))
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer)))))

;; Test kubernetes-logs-list-buffers
(ert-deftest kubernetes-logs-test-list-buffers ()
  (let* ((test-buffer1 (generate-new-buffer "*kubernetes logs: default/pod/nginx*"))
         (test-buffer2 (generate-new-buffer "*kubernetes logs: production/deployment/api:main*"))
         (test-buffers (list test-buffer1 test-buffer2))
         (selected-buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'buffer-list) (lambda () (append test-buffers (list (get-buffer-create "*scratch*")))))
                  ((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                  ((symbol-function 'completing-read)
                   (lambda (prompt collection &rest _)
                     (should collection)
                     "default/pod/nginx"))
                  ((symbol-function 'switch-to-buffer)
                   (lambda (buffer) (setq selected-buffer buffer))))
          ;; Set buffer local variables
          (with-current-buffer test-buffer1
            (setq-local kubernetes-logs-resource-type "pod")
            (setq-local kubernetes-logs-resource-name "nginx")
            (setq-local kubernetes-logs-namespace "default"))
          (with-current-buffer test-buffer2
            (setq-local kubernetes-logs-resource-type "deployment")
            (setq-local kubernetes-logs-resource-name "api")
            (setq-local kubernetes-logs-namespace "production")
            (setq-local kubernetes-logs-container-name "main"))

          ;; Run the function
          (kubernetes-logs-list-buffers)

          ;; Verify results
          (should (eq selected-buffer test-buffer1)))
      ;; Clean up test buffers
      (dolist (buf test-buffers)
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

;; Test kubernetes-logs-follow functionality
(ert-deftest kubernetes-logs-test-logs-follow ()
  (let ((test-buffer (generate-new-buffer "*kubernetes logs: default/pod/nginx*")))
    (unwind-protect
        (cl-letf (((symbol-function 'kubernetes-state--get) (lambda (&rest _) "default"))
                  ((symbol-function 'kubernetes-kubectl--flags-from-state) (lambda (&rest _) '("--context=minikube")))
                  ((symbol-function 'kubernetes-state) (lambda () '()))
                  ((symbol-function 'kubernetes-utils-process-buffer-start)
                   (lambda (buffer-name mode executable args)
                     (should (member "-f" args))
                     test-buffer))
                  ((symbol-function 'kubernetes-logs--read-resource-if-needed)
                   (lambda (state) '("pod" . "nginx")))
                  ((symbol-function 'display-buffer) (lambda (buf) (selected-window)))
                  ((symbol-function 'select-window) (lambda (&rest _) nil)))
          (condition-case err
              (progn
                (kubernetes-logs-follow '() '())
                (should t)) ; Test passes if no error
            (error (ert-fail (format "Function raised an error: %S" err)))))
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer)))))

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

;; Test keymap bindings
(ert-deftest kubernetes-logs-test-mode-keymap ()
  "Test that key bindings are properly set in kubernetes-logs-mode-map."
  (should (eq (lookup-key kubernetes-logs-mode-map (kbd "g")) 'kubernetes-logs-refresh))
  (should (eq (lookup-key kubernetes-logs-mode-map (kbd "n")) 'kubernetes-logs-forward-line))
  (should (eq (lookup-key kubernetes-logs-mode-map (kbd "p")) 'kubernetes-logs-previous-line))
  (should (eq (lookup-key kubernetes-logs-mode-map (kbd "RET")) 'kubernetes-logs-inspect-line)))

;; Test log line inspection
(ert-deftest kubernetes-logs-test-inspect-line ()
  "Test that kubernetes-logs-inspect-line works properly."
  (let ((log-buffer (generate-new-buffer "*kubernetes logs: default/pod/nginx*"))
        (line-buffer (get-buffer-create kubernetes-log-line-buffer-name)))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer) (lambda (buf) (get-buffer-window buf))))
          (with-current-buffer log-buffer
            (insert "This is a test log line\n")
            (goto-char (point-min))
            (kubernetes-logs-inspect-line (point))
            (with-current-buffer line-buffer
              (should (string= (buffer-string) "This is a test log line")))))
      (when (buffer-live-p log-buffer)
        (kill-buffer log-buffer))
      (when (buffer-live-p line-buffer)
        (kill-buffer line-buffer)))))

;;; kubernetes-logs-test.el ends here
