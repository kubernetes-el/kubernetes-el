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

;; Test resource selection and reading
(ert-deftest kubernetes-exec-test-read-resource-if-needed ()
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
                (let ((result (kubernetes-utils-read-resource-if-needed nil kubernetes-exec-supported-resource-types)))
                  (should result)
                  (should (equal (car result) "pod"))
                  (should (equal (cdr result) "test-pod")))

                ;; Test with deployment (supported resource)
                (goto-char (point-min))
                (search-forward "test-deployment")
                (backward-char 3)
                (let ((result (kubernetes-utils-read-resource-if-needed nil kubernetes-exec-supported-resource-types)))
                  (should result)
                  (should (equal (car result) "deployment"))
                  (should (equal (cdr result) "test-deployment")))

                ;; Test with service (unsupported resource - should fallback to pod)
                (goto-char (point-min))
                (search-forward "test-service")
                (backward-char 3)
                (let ((result (kubernetes-utils-read-resource-if-needed nil kubernetes-exec-supported-resource-types "pod")))
                  (should result)
                  (should (equal (car result) "pod"))
                  (should (equal (cdr result) "fallback-pod")))

                ;; Test with no resource at point (should fallback to pod)
                (goto-char (point-max))
                (let ((result (kubernetes-utils-read-resource-if-needed nil kubernetes-exec-supported-resource-types "pod")))
                  (should result)
                  (should (equal (car result) "pod"))
                  (should (equal (cdr result) "fallback-pod")))))
          (when buf (kill-buffer buf)))))))

;; Test kubernetes-exec-generate-buffer-name function
(ert-deftest kubernetes-exec-test-generate-buffer-name ()
  (cl-letf (((symbol-function 'kubernetes-state--get)
             (lambda (state key)
               (when (eq key 'current-namespace)
                 "test-namespace"))))

    ;; Test basic pod without container
    (let ((args '())
          (state '((current-namespace . "test-namespace"))))
      (should (equal (kubernetes-utils-generate-operation-buffer-name
                      "exec term" "pod" "nginx" args state)
                    "*kubernetes exec term: test-namespace/pod/nginx*")))

    ;; Test pod with container
    (let ((args '("--container=main-container"))
          (state '((current-namespace . "test-namespace"))))
      (should (equal (kubernetes-utils-generate-operation-buffer-name
                      "exec term" "pod" "nginx" args state)
                    "*kubernetes exec term: test-namespace/pod/nginx:main-container*")))

    ;; Test deployment
    (let ((args '())
          (state '((current-namespace . "test-namespace"))))
      (should (equal (kubernetes-utils-generate-operation-buffer-name
                      "exec term" "deployment" "frontend" args state)
                    "*kubernetes exec term: test-namespace/deployment/frontend*")))

    ;; Test vterm format
    (let ((args '("--container=web"))
          (state '((current-namespace . "test-namespace"))))
      (should (equal (kubernetes-utils-generate-operation-buffer-name
                      "exec vterm" "deployment" "frontend" args state)
                    "*kubernetes exec vterm: test-namespace/deployment/frontend:web*")))))

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
              ((symbol-function 'kubernetes-utils-generate-operation-buffer-name)
               (lambda (operation resource-type resource-name args state)
                 (format "*kubernetes %s: %s/%s/%s%s*"
                         operation
                         (or (kubernetes-state--get state 'current-namespace) "default")
                         resource-type
                         resource-name
                         (let ((container-arg (seq-find (lambda (arg) (string-prefix-p "--container=" arg)) args)))
                           (if container-arg
                               (format ":%s" (substring container-arg (length "--container=")))
                             "")))))
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
              ((symbol-function 'kubernetes-utils-generate-operation-buffer-name)
               (lambda (operation resource-type resource-name args state)
                 (format "*kubernetes %s: %s/%s/%s%s*"
                         operation
                         (or (kubernetes-state--get state 'current-namespace) "default")
                         resource-type
                         resource-name
                         (let ((container-arg (seq-find (lambda (arg) (string-prefix-p "--container=" arg)) args)))
                           (if container-arg
                               (format ":%s" (substring container-arg (length "--container=")))
                             "")))))
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

;; Test kubernetes-utils-get-resource-at-point for kubernetes-exec module
(ert-deftest kubernetes-exec-test--get-resource-at-point ()
  "Test that kubernetes-utils-get-resource-at-point correctly identifies resources for exec."
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
              (let ((result (kubernetes-utils-get-resource-at-point kubernetes-exec-supported-resource-types)))
                (should result)
                (should (equal (car result) "pod"))
                (should (equal (cdr result) "test-pod")))

              ;; Test with deployment (supported resource)
              (goto-char (point-min))
              (search-forward "test-deployment")
              (backward-char 3)
              (let ((result (kubernetes-utils-get-resource-at-point kubernetes-exec-supported-resource-types)))
                (should result)
                (should (equal (car result) "deployment"))
                (should (equal (cdr result) "test-deployment")))

              ;; Test with service (unsupported resource)
              (goto-char (point-min))
              (search-forward "test-service")
              (backward-char 3)
              (let ((result (kubernetes-utils-get-resource-at-point kubernetes-exec-supported-resource-types)))
                (should-not result))

              ;; Test with no resource at point
              (goto-char (point-max))
              (let ((result (kubernetes-utils-get-resource-at-point kubernetes-exec-supported-resource-types)))
                (should-not result))))
        (when buf (kill-buffer buf))))))
;; Test that kubernetes-utils-has-valid-resource-p works correctly
(ert-deftest kubernetes-exec-test--has-valid-resource-p ()
  "Test that kubernetes-utils-has-valid-resource-p correctly identifies valid resources."
  (let ((kubernetes-exec-supported-resource-types '("pod" "deployment" "statefulset" "job" "cronjob"))
        (kubernetes-utils--selected-resource nil))

    ;; Test with no resource at point and no selected resource
    (cl-letf (((symbol-function 'kubernetes-utils-get-resource-at-point)
               (lambda (types) nil)))
      (should-not (kubernetes-utils-has-valid-resource-p kubernetes-exec-supported-resource-types)))

    ;; Test with resource at point
    (cl-letf (((symbol-function 'kubernetes-utils-get-resource-at-point)
               (lambda (types) (cons "pod" "test-pod"))))
      (should (kubernetes-utils-has-valid-resource-p kubernetes-exec-supported-resource-types)))

    ;; Test with selected resource but no resource at point
    (cl-letf (((symbol-function 'kubernetes-utils-get-resource-at-point)
               (lambda (types) nil)))
      (let ((kubernetes-utils--selected-resource (cons "deployment" "test-deployment")))
        (should (kubernetes-utils-has-valid-resource-p kubernetes-exec-supported-resource-types))))))

;; Test for handling container names
(ert-deftest kubernetes-exec-test--read-container-for-resource ()
  "Test kubernetes-utils-read-container-for-resource directly."
  (let ((kubernetes-exec-supported-resource-types '("pod" "deployment" "statefulset" "job" "cronjob")))
    (cl-letf (((symbol-function 'kubernetes-utils--get-container-names)
               (lambda (type name state)
                 (should (equal type "pod"))
                 (should (equal name "test-pod"))
                 '("main-container" "sidecar")))
              ((symbol-function 'completing-read)
               (lambda (prompt choices &rest _)
                 (car choices))))

      (let ((result (kubernetes-utils-read-container-for-resource
                     "Container: " 'mock-state "pod" "test-pod" nil nil)))
        (should (equal result "main-container"))))))

;; Test for kubernetes-utils-get-current-resource-description
(ert-deftest kubernetes-exec-test--get-current-resource-description ()
  "Test that kubernetes-utils-get-current-resource-description returns correct descriptions."
  (let ((kubernetes-utils--selected-resource nil)
        (kubernetes-exec-supported-resource-types '("pod" "deployment" "statefulset" "job" "cronjob")))

    ;; Test with resource at point
    (cl-letf (((symbol-function 'kubernetes-utils-get-resource-at-point)
               (lambda (types) (cons "pod" "test-pod"))))
      (should (equal (kubernetes-utils-get-current-resource-description kubernetes-exec-supported-resource-types) "pod/test-pod")))

    ;; Test with selected resource but no resource at point
    (cl-letf (((symbol-function 'kubernetes-utils-get-resource-at-point)
               (lambda (types) nil)))
      (let ((kubernetes-utils--selected-resource (cons "deployment" "test-deployment")))
        (should (equal (kubernetes-utils-get-current-resource-description kubernetes-exec-supported-resource-types) "deployment/test-deployment"))))

    ;; Test with no resource at point and no selected resource
    (cl-letf (((symbol-function 'kubernetes-utils-get-resource-at-point)
               (lambda (types) nil)))
      (let ((kubernetes-utils--selected-resource nil))
        (should (equal (kubernetes-utils-get-current-resource-description kubernetes-exec-supported-resource-types) "selected resource"))))))

;; Test kubernetes-utils-get-effective-resource
(ert-deftest kubernetes-exec-test--get-effective-resource ()
  "Test kubernetes-utils-get-effective-resource."
  (let ((kubernetes-exec-supported-resource-types '("pod" "deployment" "statefulset" "job" "cronjob")))

    ;; 1. Test case where no resource is at point and no selected resource
    (let ((kubernetes-utils--selected-resource nil))
      (cl-letf (((symbol-function 'kubernetes-utils-get-resource-at-point)
                 (lambda (types) nil))
                ((symbol-function 'kubernetes-utils-select-resource)
                 (lambda (state types)
                   (cons "pod" "test-pod"))))
        (let ((result (kubernetes-utils-get-effective-resource nil kubernetes-exec-supported-resource-types)))
          (should (equal result (cons "pod" "test-pod")))
          (should (equal kubernetes-utils--selected-resource (cons "pod" "test-pod"))))))

    ;; 2. Test case where no resource is at point but selected resource exists
    (let ((kubernetes-utils--selected-resource (cons "deployment" "test-deployment")))
      (cl-letf (((symbol-function 'kubernetes-utils-get-resource-at-point)
                 (lambda (types) nil)))
        (let ((result (kubernetes-utils-get-effective-resource nil kubernetes-exec-supported-resource-types)))
          (should (equal result (cons "deployment" "test-deployment")))
          (should (equal kubernetes-utils--selected-resource (cons "deployment" "test-deployment"))))))

    ;; 3. Test case where resource is at point
    (let ((kubernetes-utils--selected-resource nil))
      (cl-letf (((symbol-function 'kubernetes-utils-get-resource-at-point)
                 (lambda (types) (cons "pod" "test-pod"))))
        (let ((result (kubernetes-utils-get-effective-resource nil kubernetes-exec-supported-resource-types)))
          (should (equal result (cons "pod" "test-pod")))
          (should (equal kubernetes-utils--selected-resource nil)))))))

;; Test for kubernetes-exec-vterm-with-check
(ert-deftest kubernetes-exec-test-vterm-with-check ()
  "Test kubernetes-exec-vterm-with-check function."
  (let ((kubernetes-utils--selected-resource nil)
        (kubernetes-exec-supported-resource-types '("pod" "deployment" "statefulset" "job" "cronjob")))

    ;; Test with no valid resource
    (cl-letf (((symbol-function 'kubernetes-utils-has-valid-resource-p)
               (lambda (types) nil)))
      (should-error (kubernetes-exec-vterm-with-check '() '())
                   :type 'user-error))

    ;; Test with valid resource
    (cl-letf (((symbol-function 'kubernetes-utils-has-valid-resource-p)
               (lambda (types) t))
              ((symbol-function 'kubernetes-utils-get-effective-resource)
               (lambda (state types) (cons "pod" "test-pod")))
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
                 ;; Resource path should be just "test-pod" since it's a pod
                 ;; and the function kubernetes-exec-vterm-with-check transforms it
                 ;; into "pod/test-pod" internally
                 (should (equal resource-path "test-pod"))
                 (should (equal command "/bin/bash"))
                 "vterm-executed")))

      (let ((result (kubernetes-exec-vterm-with-check '("--tty") '())))
        (should (equal result "vterm-executed"))))))

;;; kubernetes-exec-test.el ends here
