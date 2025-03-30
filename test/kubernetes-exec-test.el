;;; kubernetes-exec-test.el --- Tests for kubernetes-exec.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'cl-lib)
(require 'kubernetes-exec)
(require 'kubernetes-utils)

(defconst kubernetes-exec-test--sample-pod
  (list (cons 'metadata
              (list (cons 'name "test-pod")
                    (cons 'namespace "default")))
        (cons 'spec
              (list (cons 'containers
                          (vector (list (cons 'name "main-container"))
                                  (list (cons 'name "sidecar"))))))))

(defconst kubernetes-exec-test--sample-deployment
  (list (cons 'metadata
              (list (cons 'name "test-deployment")
                    (cons 'namespace "default")))
        (cons 'spec
              (list (cons 'template
                          (list (cons 'spec
                                      (list (cons 'containers
                                                  (vector (list (cons 'name "template-container"))))))))))))

(defconst kubernetes-exec-test--sample-job
  (list (cons 'metadata
              (list (cons 'name "test-job")
                    (cons 'namespace "default")))
        (cons 'spec
              (list (cons 'template
                          (list (cons 'spec
                                      (list (cons 'containers
                                                  (vector (list (cons 'name "job-container"))))))))))))

(defconst kubernetes-exec-test--sample-pods
  (list (cons 'items
              (vector kubernetes-exec-test--sample-pod
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

;; Test kubernetes-exec--read-resource-if-needed for different resource types
(ert-deftest kubernetes-exec-test--read-resource-if-needed ()
  (let ((kubernetes-overview-buffer-name "*kubernetes-test-overview*")
        (kubernetes-exec-supported-resource-types '("pod" "deployment" "statefulset" "job" "cronjob")))

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
                (let ((result (kubernetes-exec--read-resource-if-needed nil)))
                  (should result)
                  (should (equal (car result) "pod"))
                  (should (equal (cdr result) "test-pod")))

                ;; Test with deployment (supported resource)
                (goto-char (point-min))
                (search-forward "test-deployment")
                (backward-char 3)
                (let ((result (kubernetes-exec--read-resource-if-needed nil)))
                  (should result)
                  (should (equal (car result) "deployment"))
                  (should (equal (cdr result) "test-deployment")))

                ;; Test with service (unsupported resource - should fallback to pod)
                (goto-char (point-min))
                (search-forward "test-service")
                (backward-char 3)
                (let ((result (kubernetes-exec--read-resource-if-needed nil)))
                  (should result)
                  (should (equal (car result) "pod"))
                  (should (equal (cdr result) "fallback-pod")))

                ;; Test with no resource at point (should fallback to pod)
                (goto-char (point-max))
                (let ((result (kubernetes-exec--read-resource-if-needed nil)))
                  (should result)
                  (should (equal (car result) "pod"))
                  (should (equal (cdr result) "fallback-pod")))))
          (when buf (kill-buffer buf)))))))

;; Test kubernetes-exec--generate-buffer-name function
(ert-deftest kubernetes-exec-test-generate-buffer-name ()
  ;; First, ensure that namespace detection works properly
  (cl-letf (((symbol-function 'kubernetes-state--get)
             (lambda (state key)
               (when (eq key 'current-namespace)
                 "test-namespace"))))

    ;; Test basic pod without container
    (let ((args '())
          (state 'mock-state))
      (should (equal (kubernetes-exec--generate-buffer-name "pod" "nginx" args state)
                    "*kubernetes exec term: test-namespace/pod/nginx*")))

    ;; Test pod with container
    (let ((args '("--container=app"))
          (state 'mock-state))
      (should (equal (kubernetes-exec--generate-buffer-name "pod" "nginx" args state)
                    "*kubernetes exec term: test-namespace/pod/nginx:app*")))

    ;; Test deployment
    (let ((args '())
          (state 'mock-state))
      (should (equal (kubernetes-exec--generate-buffer-name "deployment" "frontend" args state)
                    "*kubernetes exec term: test-namespace/deployment/frontend*")))

    ;; Test deployment with container
    (let ((args '("--container=web"))
          (state 'mock-state))
      (should (equal (kubernetes-exec--generate-buffer-name "deployment" "frontend" args state)
                    "*kubernetes exec term: test-namespace/deployment/frontend:web*")))

    ;; Test vterm buffer name format
    (let ((args '())
          (state 'mock-state))
      (should (equal (kubernetes-exec--generate-buffer-name "pod" "nginx" args state t)
                    "*kubernetes exec vterm: test-namespace/pod/nginx*")))))

;; Test kubernetes-exec--exec-command-generation
(ert-deftest kubernetes-exec-test--exec-command-generation ()
  "Test that kubectl commands are constructed correctly for different resources."
  (let ((commands-executed nil))
    (cl-letf (((symbol-function 'kubernetes-kubectl--flags-from-state)
               (lambda (_) '("--kubeconfig=/test/config")))
              ((symbol-function 'kubernetes-state--get)
               (lambda (_ key) (when (eq key 'current-namespace) "test-namespace")))
              ((symbol-function 'kubernetes-utils-term-buffer-start)
               (lambda (buffer-name executable args)
                 ;; Store the command for verification
                 (push (list buffer-name executable args) commands-executed)
                 ;; Return a dummy buffer
                 (get-buffer-create "*test-buffer*")))
              ((symbol-function 'kubernetes-utils-process-buffer-start)
               (lambda (buffer-name mode executable args)
                 ;; Store the command for verification
                 (push (list buffer-name executable args) commands-executed)
                 ;; Return a dummy buffer
                 (get-buffer-create "*test-buffer*")))
              ((symbol-function 'get-buffer-process) (lambda (_) nil))
              ((symbol-function 'set-process-sentinel) #'ignore)
              ((symbol-function 'select-window) #'ignore)
              ((symbol-function 'display-buffer) #'identity))

      (unwind-protect
          (progn
            ;; Test pod exec command
            (kubernetes-exec--exec-internal "pod" "test-pod" '() "/bin/bash" 'mock-state)

            ;; Test pod exec command with container
            (kubernetes-exec--exec-internal "pod" "test-pod" '("--container=main-container") "/bin/bash" 'mock-state)

            ;; Test pod exec command with TTY
            (kubernetes-exec--exec-internal "pod" "test-pod" '("--tty") "/bin/bash" 'mock-state)

            ;; Test deployment exec command
            (kubernetes-exec--exec-internal "deployment" "test-deployment" '() "/bin/bash" 'mock-state)

            ;; Test job exec command with container
            (kubernetes-exec--exec-internal "job" "test-job" '("--container=job-container") "/bin/bash" 'mock-state)

            ;; Now verify all the commands (commands are pushed in reverse order)
            (let ((job-cmd (pop commands-executed))
                  (deployment-cmd (pop commands-executed))
                  (pod-tty-cmd (pop commands-executed))
                  (pod-container-cmd (pop commands-executed))
                  (pod-cmd (pop commands-executed)))

              ;; Verify pod command
              (should (string-match-p "\\*kubernetes exec term: test-namespace/pod/test-pod\\*"
                                      (nth 0 pod-cmd)))
              (should (equal (nth 1 pod-cmd) kubernetes-kubectl-executable))
              (let ((args (nth 2 pod-cmd)))
                (should (equal (nth 0 args) "exec"))
                (should (member "pod/test-pod" args))
                (should (member "--namespace=test-namespace" args))
                (should (member "--" args))
                (should (member "/bin/bash" args)))

              ;; Verify pod with container command
              (should (string-match-p "\\*kubernetes exec term: test-namespace/pod/test-pod:main-container\\*"
                                      (nth 0 pod-container-cmd)))
              (let ((args (nth 2 pod-container-cmd)))
                (should (equal (nth 0 args) "exec"))
                (should (member "--container=main-container" args))
                (should (member "pod/test-pod" args)))

              ;; Verify pod with TTY command
              (should (string-match-p "\\*kubernetes exec term: test-namespace/pod/test-pod\\*"
                                      (nth 0 pod-tty-cmd)))
              (let ((args (nth 2 pod-tty-cmd)))
                (should (equal (nth 0 args) "exec"))
                (should (member "--tty" args))
                (should (member "pod/test-pod" args)))

              ;; Verify deployment command
              (should (string-match-p "\\*kubernetes exec term: test-namespace/deployment/test-deployment\\*"
                                      (nth 0 deployment-cmd)))
              (let ((args (nth 2 deployment-cmd)))
                (should (equal (nth 0 args) "exec"))
                (should (member "deployment/test-deployment" args))
                (should (member "--namespace=test-namespace" args)))

              ;; Verify job with container command
              (should (string-match-p "\\*kubernetes exec term: test-namespace/job/test-job:job-container\\*"
                                      (nth 0 job-cmd)))
              (let ((args (nth 2 job-cmd)))
                (should (equal (nth 0 args) "exec"))
                (should (member "--container=job-container" args))
                (should (member "job/test-job" args)))))

        ;; Clean up any temp buffers
        (when (get-buffer "*test-buffer*")
          (kill-buffer "*test-buffer*"))))))

;; Test kubernetes-exec-into with different resource formats
(ert-deftest kubernetes-exec-test-exec-into-with-different-resources ()
  "Test that kubernetes-exec-into handles both pod names and resource/name formats."
  (let ((commands-executed nil))
    (cl-letf (((symbol-function 'kubernetes-kubectl--flags-from-state)
               (lambda (_) '("--kubeconfig=/test/config")))
              ((symbol-function 'kubernetes-state--get)
               (lambda (_ key) (when (eq key 'current-namespace) "test-namespace")))
              ((symbol-function 'kubernetes-utils-term-buffer-start)
               (lambda (buffer-name executable args)
                 ;; Store the command for verification
                 (push (list buffer-name executable args) commands-executed)
                 ;; Return a dummy buffer
                 (get-buffer-create "*test-buffer*")))
              ((symbol-function 'kubernetes-utils-process-buffer-start)
               (lambda (buffer-name mode executable args)
                 ;; Store the command for verification
                 (push (list buffer-name executable args) commands-executed)
                 ;; Return a dummy buffer
                 (get-buffer-create "*test-buffer*")))
              ((symbol-function 'get-buffer-process) #'ignore)
              ((symbol-function 'set-process-sentinel) #'ignore)
              ((symbol-function 'select-window) #'ignore)
              ((symbol-function 'display-buffer) #'identity))

      (unwind-protect
          (progn
            ;; Test normal pod name
            (kubernetes-exec-into "test-pod" '() "/bin/bash" 'mock-state)

            ;; Test resource/name format for a deployment
            (kubernetes-exec-into "deployment/test-deployment" '() "/bin/bash" 'mock-state)

            ;; Test resource/name format for a job
            (kubernetes-exec-into "job/test-job" '("--container=job-container") "/bin/bash" 'mock-state)

            ;; Now verify all the commands (commands are pushed in reverse order)
            (let ((job-cmd (pop commands-executed))
                  (deployment-cmd (pop commands-executed))
                  (pod-cmd (pop commands-executed)))

              ;; Verify pod command treats it as a pod
              (should (string-match-p "\\*kubernetes exec term: test-namespace/pod/test-pod\\*"
                                      (nth 0 pod-cmd)))
              (should (member "pod/test-pod" (nth 2 pod-cmd)))

              ;; Verify deployment format is parsed correctly
              (should (string-match-p "\\*kubernetes exec term: test-namespace/deployment/test-deployment\\*"
                                      (nth 0 deployment-cmd)))
              (should (member "deployment/test-deployment" (nth 2 deployment-cmd)))

              ;; Verify job with container is parsed correctly
              (should (string-match-p "\\*kubernetes exec term: test-namespace/job/test-job:job-container\\*"
                                      (nth 0 job-cmd)))
              (should (member "job/test-job" (nth 2 job-cmd)))
              (should (member "--container=job-container" (nth 2 job-cmd)))))

        ;; Clean up any temp buffers
        (when (get-buffer "*test-buffer*")
          (kill-buffer "*test-buffer*"))))))

;; Test kubernetes-exec--get-resource-at-point
(ert-deftest kubernetes-exec-test--get-resource-at-point ()
  "Test that kubernetes-exec--get-resource-at-point correctly identifies resources."
  (let ((kubernetes-overview-buffer-name "*kubernetes-test-overview*")
        (kubernetes-exec-supported-resource-types '("pod" "deployment" "statefulset" "job" "cronjob")))

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
              (let ((result (kubernetes-exec--get-resource-at-point)))
                (should result)
                (should (equal (car result) "pod"))
                (should (equal (cdr result) "test-pod")))

              ;; Test with deployment (supported resource)
              (goto-char (point-min))
              (search-forward "test-deployment")
              (backward-char 3)
              (let ((result (kubernetes-exec--get-resource-at-point)))
                (should result)
                (should (equal (car result) "deployment"))
                (should (equal (cdr result) "test-deployment")))

              ;; Test with service (unsupported resource)
              (goto-char (point-min))
              (search-forward "test-service")
              (backward-char 3)
              (let ((result (kubernetes-exec--get-resource-at-point)))
                (should-not result))

              ;; Test with no resource at point
              (goto-char (point-max))
              (let ((result (kubernetes-exec--get-resource-at-point)))
                (should-not result))))
        (when buf (kill-buffer buf))))))

;; Test kubernetes-exec-using-vterm function
(ert-deftest kubernetes-exec-test-using-vterm ()
  "Test that kubernetes-exec-using-vterm properly builds the command."
  (let ((commands-executed nil))
    (cl-letf (((symbol-function 'kubernetes-kubectl--flags-from-state)
               (lambda (_) '("--kubeconfig=/test/config")))
              ((symbol-function 'kubernetes-state--get)
               (lambda (_ key) (when (eq key 'current-namespace) "test-namespace")))
              ((symbol-function 'kubernetes-utils-vterm-start)
               (lambda (buffer-name executable args)
                 ;; Store the command for verification
                 (push (list buffer-name executable args) commands-executed)
                 ;; Return a dummy buffer
                 (get-buffer-create "*test-buffer*")))
              ((symbol-function 'vterm-other-window)
               (lambda () (get-buffer-create "*test-vterm-buffer*")))
              ((symbol-function 'require) (lambda (feature &rest _) t))
              ((symbol-function 'select-window) #'ignore)
              ((symbol-function 'display-buffer) #'identity))

      (unwind-protect
          (progn
            ;; Test normal pod name
            (kubernetes-exec-using-vterm "test-pod" '("--tty") "/bin/bash" 'mock-state)

            ;; Test resource/name format for a deployment
            (kubernetes-exec-using-vterm "deployment/test-deployment" '("--tty") "/bin/bash" 'mock-state)

            ;; Now verify all the commands (commands are pushed in reverse order)
            (let ((deployment-cmd (pop commands-executed))
                  (pod-cmd (pop commands-executed)))

              ;; Verify pod command uses vterm buffer name format
              (should (string-match-p "\\*kubernetes exec vterm: test-namespace/pod/test-pod\\*"
                                     (nth 0 pod-cmd)))
              (should (equal (nth 1 pod-cmd) kubernetes-kubectl-executable))
              (let ((args (nth 2 pod-cmd)))
                (should (equal (nth 0 args) "exec"))
                (should (member "--tty" args))
                (should (member "test-pod" args))
                (should (member "--namespace=test-namespace" args))
                (should (member "--" args))
                (should (member "/bin/bash" args)))

              ;; Verify deployment command uses vterm buffer name format
              (should (string-match-p "\\*kubernetes exec vterm: test-namespace/deployment/test-deployment\\*"
                                     (nth 0 deployment-cmd)))
              (let ((args (nth 2 deployment-cmd)))
                (should (equal (nth 0 args) "exec"))
                (should (member "--tty" args))
                (should (member "deployment/test-deployment" args))
                (should (member "--namespace=test-namespace" args))
                (should (member "--" args))
                (should (member "/bin/bash" args)))))

        ;; Clean up any temp buffers
        (when (get-buffer "*test-buffer*")
          (kill-buffer "*test-buffer*"))
        (when (get-buffer "*test-vterm-buffer*")
          (kill-buffer "*test-vterm-buffer*"))))))

;; Test for kubernetes-exec--read-container-for-selected-resource
(ert-deftest kubernetes-exec-test--read-container-for-selected-resource ()
  "Test kubernetes-exec--read-container-for-selected-resource directly."
  (let ((kubernetes-exec-supported-resource-types '("pod" "deployment" "statefulset" "job" "cronjob")))
    (cl-letf (((symbol-function 'kubernetes-exec--get-effective-resource)
               (lambda (_) '("pod" . "test-pod")))
              ((symbol-function 'kubernetes-utils--get-container-names)
               (lambda (type name state)
                 '("main-container" "sidecar")))
              ((symbol-function 'completing-read)
               (lambda (prompt choices &rest _)
                 (car choices))))

      (let ((result (kubernetes-exec--read-container-for-selected-resource "Container: " nil nil)))
        (should (equal result "main-container"))))))

;; Test command wrapper with check
(ert-deftest kubernetes-exec-test-into-with-check ()
  "Test kubernetes-exec-into-with-check function."
  (let ((kubernetes-exec--selected-resource nil))

    ;; Test with no valid resource
    (cl-letf (((symbol-function 'kubernetes-exec--has-valid-resource-p)
               (lambda () nil)))
      (should-error (kubernetes-exec-into-with-check '() '())
                   :type 'user-error))

    ;; Test with valid resource - pod type
    (cl-letf (((symbol-function 'kubernetes-exec--has-valid-resource-p)
               (lambda () t))
              ((symbol-function 'kubernetes-state)
               (lambda () '((current-namespace . "default"))))
              ((symbol-function 'kubernetes-exec--get-effective-resource)
               (lambda (_) (cons "pod" "test-pod")))
              ((symbol-function 'read-string)
               (lambda (prompt &rest _) "/bin/bash"))
              ((symbol-function 'kubernetes-exec-into)
               (lambda (resource args command state)
                 ;; For pods, only the name is passed, NOT in "pod/name" format
                 (should (equal resource "test-pod"))
                 (should (equal args '("--tty")))
                 (should (equal command "/bin/bash"))
                 "success")))

      (let ((result (kubernetes-exec-into-with-check '("--tty") '())))
        (should (equal result "success"))))

    ;; Test with valid resource - non-pod type (like deployment)
    (cl-letf (((symbol-function 'kubernetes-exec--has-valid-resource-p)
               (lambda () t))
              ((symbol-function 'kubernetes-state)
               (lambda () '((current-namespace . "default"))))
              ((symbol-function 'kubernetes-exec--get-effective-resource)
               (lambda (_) (cons "deployment" "test-deployment")))
              ((symbol-function 'read-string)
               (lambda (prompt &rest _) "/bin/bash"))
              ((symbol-function 'kubernetes-exec-into)
               (lambda (resource args command state)
                 ;; For non-pods, the format is "type/name"
                 (should (equal resource "deployment/test-deployment"))
                 (should (equal args '("--tty")))
                 (should (equal command "/bin/bash"))
                 "success")))

      (let ((result (kubernetes-exec-into-with-check '("--tty") '())))
        (should (equal result "success"))))))

;; Test for kubernetes-exec-vterm-with-check
(ert-deftest kubernetes-exec-test-vterm-with-check ()
  "Test kubernetes-exec-vterm-with-check."
  (let ((kubernetes-exec--selected-resource nil))

    ;; Test with no valid resource (should error)
    (cl-letf (((symbol-function 'kubernetes-exec--has-valid-resource-p)
               (lambda () nil)))

      (should-error (kubernetes-exec-vterm-with-check '() '()) :type 'user-error))

    ;; Test with pod resource - note the resource path for pods is just the name, not "pod/name"
    (cl-letf (((symbol-function 'kubernetes-exec--has-valid-resource-p)
               (lambda () t))
              ((symbol-function 'kubernetes-state)
               (lambda () '((current-namespace . "default"))))
              ((symbol-function 'kubernetes-exec--get-effective-resource)
               (lambda (_) '("pod" . "test-pod")))
              ((symbol-function 'read-string)
               (lambda (prompt &rest _) "/bin/bash"))
              ((symbol-function 'string-empty-p)
               (lambda (s) (equal s "")))
              ((symbol-function 'string-trim)
               (lambda (s) s))
              ((symbol-function 'require)
               (lambda (feature &optional noerror) t))
              ((symbol-function 'kubernetes-exec-using-vterm)
               (lambda (resource-path args command state)
                 ;; For pods, just the name is passed, NOT in "pod/name" format
                 (should (equal resource-path "test-pod"))
                 (should (equal command "/bin/bash"))
                 "vterm-executed")))

      (let ((result (kubernetes-exec-vterm-with-check '("--tty") '())))
        (should (equal result "vterm-executed"))))

    ;; Test with non-pod resource (e.g., deployment)
    (cl-letf (((symbol-function 'kubernetes-exec--has-valid-resource-p)
               (lambda () t))
              ((symbol-function 'kubernetes-state)
               (lambda () '((current-namespace . "default"))))
              ((symbol-function 'kubernetes-exec--get-effective-resource)
               (lambda (_) '("deployment" . "test-deployment")))
              ((symbol-function 'read-string)
               (lambda (prompt &rest _) "/bin/bash"))
              ((symbol-function 'string-empty-p)
               (lambda (s) (equal s "")))
              ((symbol-function 'string-trim)
               (lambda (s) s))
              ((symbol-function 'require)
               (lambda (feature &optional noerror) t))
              ((symbol-function 'kubernetes-exec-using-vterm)
               (lambda (resource-path args command state)
                 ;; For non-pods, the format is "type/name"
                 (should (equal resource-path "deployment/test-deployment"))
                 (should (equal command "/bin/bash"))
                 "vterm-executed")))

      (let ((result (kubernetes-exec-vterm-with-check '("--tty") '())))
        (should (equal result "vterm-executed"))))))

;; Test for kubernetes-exec--read-container-for-selected-resource with empty containers
(ert-deftest kubernetes-exec-test--read-container-for-selected-resource-empty ()
  "Test kubernetes-exec--read-container-for-selected-resource with empty containers."
  (let ((kubernetes-exec-supported-resource-types '("pod" "deployment" "statefulset" "job" "cronjob")))
    (cl-letf (((symbol-function 'kubernetes-exec--get-effective-resource)
               (lambda (_) '("pod" . "empty-pod")))
              ((symbol-function 'kubernetes-utils--get-container-names)
               (lambda (type name state)
                 '())))

      (should-error (kubernetes-exec--read-container-for-selected-resource "Container: " nil nil)))))

;; Test for kubernetes-exec-into-with-check with empty command
(ert-deftest kubernetes-exec-test-into-with-check-empty-command ()
  "Test kubernetes-exec-into-with-check with empty command."
  (let ((kubernetes-exec--selected-resource nil)
        (kubernetes-default-exec-command "/bin/bash"))

    ;; Test with valid resource and empty command (should use default)
    (cl-letf (((symbol-function 'kubernetes-exec--has-valid-resource-p)
               (lambda () t))
              ((symbol-function 'kubernetes-state)
               (lambda () '((current-namespace . "default"))))
              ((symbol-function 'kubernetes-exec--get-effective-resource)
               (lambda (_) '("pod" . "test-pod")))
              ((symbol-function 'read-string)
               (lambda (prompt &rest _) ""))
              ((symbol-function 'string-empty-p)
               (lambda (s) (equal s "")))
              ((symbol-function 'string-trim)
               (lambda (s) s))
              ((symbol-function 'kubernetes-exec-into)
               (lambda (resource-path args command state)
                 (should (equal resource-path "test-pod"))
                 (should (equal command "/bin/bash"))  ;; Should use default command
                 "default-command-used")))

      (let ((result (kubernetes-exec-into-with-check '("--tty") '())))
        (should (equal result "default-command-used"))))))

;; Test for kubernetes-exec-vterm-with-check with empty command
(ert-deftest kubernetes-exec-test-vterm-with-check-empty-command ()
  "Test kubernetes-exec-vterm-with-check with empty command."
  (let ((kubernetes-exec--selected-resource nil)
        (kubernetes-default-exec-command "/bin/bash"))

    ;; Test with valid resource and empty command (should use default)
    (cl-letf (((symbol-function 'kubernetes-exec--has-valid-resource-p)
               (lambda () t))
              ((symbol-function 'kubernetes-state)
               (lambda () '((current-namespace . "default"))))
              ((symbol-function 'kubernetes-exec--get-effective-resource)
               (lambda (_) '("pod" . "test-pod")))
              ((symbol-function 'read-string)
               (lambda (prompt &rest _) ""))
              ((symbol-function 'string-empty-p)
               (lambda (s) (equal s "")))
              ((symbol-function 'string-trim)
               (lambda (s) s))
              ((symbol-function 'require)
               (lambda (feature &optional noerror) t))
              ((symbol-function 'kubernetes-exec-using-vterm)
               (lambda (resource-path args command state)
                 (should (equal resource-path "test-pod"))
                 (should (equal command "/bin/bash"))  ;; Should use default command
                 "default-command-used")))

      (let ((result (kubernetes-exec-vterm-with-check '("--tty") '())))
        (should (equal result "default-command-used"))))))

;; Test for kubernetes-exec--get-current-resource-description
(ert-deftest kubernetes-exec-test--get-current-resource-description ()
  "Test kubernetes-exec--get-current-resource-description."
  (let ((kubernetes-exec--selected-resource nil))

    ;; Test when resource is at point
    (cl-letf (((symbol-function 'kubernetes-exec--get-resource-at-point)
               (lambda () '("pod" . "test-pod"))))

      (let ((desc (kubernetes-exec--get-current-resource-description)))
        (should (equal desc "pod/test-pod"))))

    ;; Test when no resource at point but selected resource exists
    (setq kubernetes-exec--selected-resource '("deployment" . "test-deployment"))
    (cl-letf (((symbol-function 'kubernetes-exec--get-resource-at-point)
               (lambda () nil)))

      (let ((desc (kubernetes-exec--get-current-resource-description)))
        (should (equal desc "deployment/test-deployment"))))

    ;; Test when no resource at point and no selected resource
    (setq kubernetes-exec--selected-resource nil)
    (cl-letf (((symbol-function 'kubernetes-exec--get-resource-at-point)
               (lambda () nil)))

      (let ((desc (kubernetes-exec--get-current-resource-description)))
        (should (equal desc "selected resource"))))))

;; Test for kubernetes-exec-select-resource
(ert-deftest kubernetes-exec-test-select-resource ()
  "Test kubernetes-exec-select-resource."
  (let ((kubernetes-exec--selected-resource nil)
        (kubernetes-exec-supported-resource-types '("pod" "deployment" "statefulset" "job" "cronjob")))

    ;; Mock functions to avoid interactive prompts and actual operations
    (cl-letf (((symbol-function 'kubernetes-state)
               (lambda () '((current-namespace . "default"))))
              ((symbol-function 'kubernetes-utils-select-resource)
               (lambda (state types)
                 (should (equal types kubernetes-exec-supported-resource-types))
                 (cons "pod" "test-pod")))
              ((symbol-function 'message)
               (lambda (format-string &rest args) nil))
              ((symbol-function 'transient-setup)
               (lambda (command) nil)))

      ;; Call the function and check the resource is stored
      (kubernetes-exec-select-resource)
      (should (equal kubernetes-exec--selected-resource '("pod" . "test-pod"))))))

;; Test for kubernetes-exec--get-effective-resource
(ert-deftest kubernetes-exec-test--get-effective-resource ()
  "Test ~kubernetes-exec--get-effective-resource~."
  (let ((kubernetes-exec-supported-resource-types '("pod" "deployment" "statefulset" "job" "cronjob")))

    ;; 1. Test case where no resource is at point and no selected resource
    (let ((kubernetes-exec--selected-resource nil))
      (cl-letf (((symbol-function 'kubernetes-exec--get-resource-at-point)
                 (lambda () nil))
                ((symbol-function 'kubernetes-utils-select-resource)
                 (lambda (_state _types)
                   '("pod" . "test-pod"))))
        (let ((result (kubernetes-exec--get-effective-resource nil)))
          (should (equal result '("pod" . "test-pod")))
          (should (equal kubernetes-exec--selected-resource '("pod" . "test-pod"))))))

    ;; 2. Test case where no resource is at point but selected resource exists
    (let ((kubernetes-exec--selected-resource '("statefulset" . "test-statefulset")))
      (cl-letf (((symbol-function 'kubernetes-exec--get-resource-at-point)
                 (lambda () nil)))
        (let ((result (kubernetes-exec--get-effective-resource nil)))
          (should (equal result '("statefulset" . "test-statefulset")))
          (should (equal kubernetes-exec--selected-resource '("statefulset" . "test-statefulset"))))))

    ;; 3. Test case where resource is at point
    (let ((kubernetes-exec--selected-resource nil))
      (cl-letf (((symbol-function 'kubernetes-exec--get-resource-at-point)
                 (lambda () '("pod" . "test-pod"))))
        (let ((result (kubernetes-exec--get-effective-resource nil)))
          (should (equal result '("pod" . "test-pod")))
          (should (equal kubernetes-exec--selected-resource nil)))))))

;;; kubernetes-exec-test.el ends here
