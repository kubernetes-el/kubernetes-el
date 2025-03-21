;;; kubernetes-exec-test.el --- Tests for kubernetes-exec.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
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
                    "*kubernetes exec: test-namespace/pod/nginx*")))

    ;; Test pod with container
    (let ((args '("--container=app"))
          (state 'mock-state))
      (should (equal (kubernetes-exec--generate-buffer-name "pod" "nginx" args state)
                    "*kubernetes exec: test-namespace/pod/nginx:app*")))

    ;; Test deployment
    (let ((args '())
          (state 'mock-state))
      (should (equal (kubernetes-exec--generate-buffer-name "deployment" "frontend" args state)
                    "*kubernetes exec: test-namespace/deployment/frontend*")))

    ;; Test deployment with container
    (let ((args '("--container=web"))
          (state 'mock-state))
      (should (equal (kubernetes-exec--generate-buffer-name "deployment" "frontend" args state)
                    "*kubernetes exec: test-namespace/deployment/frontend:web*")))

    ;; Test vterm buffer name format
    (let ((args '())
          (state 'mock-state))
      (should (equal (kubernetes-exec--generate-buffer-name "pod" "nginx" args state t)
                    "*kubernetes vterm exec: test-namespace/pod/nginx*")))))

(ert-deftest kubernetes-exec-test--exec-command-generation ()
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
                 (let ((buf (generate-new-buffer "*test-buffer*")))
                   ;; Create a mock process
                   (with-current-buffer buf
                     (set-process-query-on-exit-flag (start-process "test-proc" buf "echo" "dummy") nil))
                   buf)))
              ((symbol-function 'kubernetes-utils-term-buffer-start)
               (lambda (buffer-name executable args)
                 ;; Store the command for verification
                 (push (list buffer-name executable args) commands-executed)
                 ;; Return a dummy buffer
                 (let ((buf (generate-new-buffer "*test-buffer*")))
                   ;; Create a mock process
                   (with-current-buffer buf
                     (set-process-query-on-exit-flag (start-process "test-proc" buf "echo" "dummy") nil))
                   buf)))
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
              (should (string-match-p "\\*kubernetes exec: test-namespace/pod/test-pod\\*" (nth 0 pod-cmd)))
              (should (equal (nth 1 pod-cmd) kubernetes-kubectl-executable))
              (let ((args (nth 2 pod-cmd)))
                (should (equal (nth 0 args) "exec"))
                (should (member "test-pod" args))
                (should (member "--namespace=test-namespace" args))
                (should (member "--" args))
                (should (member "/bin/bash" args)))

              ;; Verify pod with container command
              (should (string-match-p "\\*kubernetes exec: test-namespace/pod/test-pod:main-container\\*"
                                      (nth 0 pod-container-cmd)))
              (let ((args (nth 2 pod-container-cmd)))
                (should (equal (nth 0 args) "exec"))
                (should (member "--container=main-container" args))
                (should (member "test-pod" args)))

              ;; Verify pod with TTY command
              (should (string-match-p "\\*kubernetes exec: test-namespace/pod/test-pod\\*"
                                      (nth 0 pod-tty-cmd)))
              (let ((args (nth 2 pod-tty-cmd)))
                (should (equal (nth 0 args) "exec"))
                (should (member "--tty" args))
                (should (member "test-pod" args)))

              ;; Verify deployment command
              (should (string-match-p "\\*kubernetes exec: test-namespace/deployment/test-deployment\\*"
                                      (nth 0 deployment-cmd)))
              (let ((args (nth 2 deployment-cmd)))
                (should (equal (nth 0 args) "exec"))
                (should (member "deployment/test-deployment" args))
                (should (member "--namespace=test-namespace" args)))

              ;; Verify job with container command
              (should (string-match-p "\\*kubernetes exec: test-namespace/job/test-job:job-container\\*"
                                      (nth 0 job-cmd)))
              (let ((args (nth 2 job-cmd)))
                (should (equal (nth 0 args) "exec"))
                (should (member "--container=job-container" args))
                (should (member "job/test-job" args)))))

        ;; Clean up any temp buffers
        (let ((test-buffer (get-buffer "*test-buffer*")))
          (when (buffer-live-p test-buffer)
            (kill-buffer test-buffer)))))))

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
                 (let ((buf (generate-new-buffer "*test-buffer*")))
                   ;; Create a mock process
                   (with-current-buffer buf
                     (set-process-query-on-exit-flag (start-process "test-proc" buf "echo" "dummy") nil))
                   buf)))
              ((symbol-function 'kubernetes-utils-process-buffer-start)
               (lambda (buffer-name mode executable args)
                 ;; Store the command for verification
                 (push (list buffer-name executable args) commands-executed)
                 ;; Return a dummy buffer
                 (let ((buf (generate-new-buffer "*test-buffer*")))
                   ;; Create a mock process
                   (with-current-buffer buf
                     (set-process-query-on-exit-flag (start-process "test-proc" buf "echo" "dummy") nil))
                   buf)))
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
              (should (string-match-p "\\*kubernetes exec: test-namespace/pod/test-pod\\*"
                                      (nth 0 pod-cmd)))
              (should (member "test-pod" (nth 2 pod-cmd)))

              ;; Verify deployment format is parsed correctly
              (should (string-match-p "\\*kubernetes exec: test-namespace/deployment/test-deployment\\*"
                                      (nth 0 deployment-cmd)))
              (should (member "deployment/test-deployment" (nth 2 deployment-cmd)))

              ;; Verify job with container is parsed correctly
              (should (string-match-p "\\*kubernetes exec: test-namespace/job/test-job:job-container\\*"
                                      (nth 0 job-cmd)))
              (should (member "job/test-job" (nth 2 job-cmd)))
              (should (member "--container=job-container" (nth 2 job-cmd)))))

        ;; Clean up any temp buffers
        (let ((test-buffer (get-buffer "*test-buffer*")))
          (when (buffer-live-p test-buffer)
            (kill-buffer test-buffer)))))))

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
                 (generate-new-buffer "*test-buffer*")))
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
              (should (string-match-p "\\*kubernetes vterm exec: test-namespace/pod/test-pod\\*"
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
              (should (string-match-p "\\*kubernetes vterm exec: test-namespace/deployment/test-deployment\\*"
                                     (nth 0 deployment-cmd)))
              (let ((args (nth 2 deployment-cmd)))
                (should (equal (nth 0 args) "exec"))
                (should (member "--tty" args))
                (should (member "deployment/test-deployment" args))
                (should (member "--namespace=test-namespace" args))
                (should (member "--" args))
                (should (member "/bin/bash" args)))))

        ;; Clean up any temp buffers
        (let ((test-buffer (get-buffer "*test-buffer*")))
          (when (buffer-live-p test-buffer)
            (kill-buffer test-buffer)))))))

;; Test kubernetes-exec-list-buffers
(ert-deftest kubernetes-exec-test-list-buffers ()
  "Test that kubernetes-exec-list-buffers properly lists the exec buffers."
  (let* ((test-buffer1 (generate-new-buffer "*kubernetes exec: default/pod/nginx*"))
         (test-buffer2 (generate-new-buffer "*kubernetes exec: production/deployment/api:main*"))
         (test-buffer3 (generate-new-buffer "*kubernetes vterm exec: default/statefulset/db*"))
         (test-buffers (list test-buffer1 test-buffer2 test-buffer3))
         (selected-buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'buffer-list)
                   (lambda () (append test-buffers (list (get-buffer-create "*scratch*")))))
                  ((symbol-function 'completing-read)
                   (lambda (prompt collection &rest _)
                     (should collection)
                     "default/pod/nginx: /bin/bash"))
                  ((symbol-function 'switch-to-buffer)
                   (lambda (buffer) (setq selected-buffer buffer))))

          ;; Set buffer local variables
          (with-current-buffer test-buffer1
            (setq-local kubernetes-exec-resource-type "pod")
            (setq-local kubernetes-exec-resource-name "nginx")
            (setq-local kubernetes-exec-namespace "default")
            (setq-local kubernetes-exec-command "/bin/bash"))

          (with-current-buffer test-buffer2
            (setq-local kubernetes-exec-resource-type "deployment")
            (setq-local kubernetes-exec-resource-name "api")
            (setq-local kubernetes-exec-namespace "production")
            (setq-local kubernetes-exec-container-name "main")
            (setq-local kubernetes-exec-command "/bin/sh"))

          (with-current-buffer test-buffer3
            (setq-local kubernetes-exec-resource-type "statefulset")
            (setq-local kubernetes-exec-resource-name "db")
            (setq-local kubernetes-exec-namespace "default")
            (setq-local kubernetes-exec-command "mysql"))

          ;; Run the function
          (kubernetes-exec-list-buffers)

          ;; Verify results
          (should (eq selected-buffer test-buffer1)))

      ;; Clean up test buffers
      (dolist (buf test-buffers)
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(provide 'kubernetes-exec-test)

;;; kubernetes-exec-test.el ends here
