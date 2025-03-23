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
        (kubernetes-logs-supported-resource-types '("pod" "deployment" "statefulset" "job" "service")))

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
                (insert "Ingress: ")
                (let ((start (point))
                      (ingress-name "test-ingress"))
                  (insert ingress-name)
                  (let ((nav-property (list :ingress-name ingress-name)))
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

                ;; Test with ingress (unsupported resource - should fallback to pod)
                (goto-char (point-min))
                (search-forward "test-ingress")
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
                (should (member "pod/test-pod" args))
                (should (member "--namespace=test-namespace" args)))

              ;; Verify pod with container command
              (should (string-match-p "\\*kubernetes logs: test-namespace/pod/test-pod:main-container\\*" (nth 0 pod-container-cmd)))
              (let ((args (nth 2 pod-container-cmd)))
                (should (equal (nth 0 args) "logs"))
                (should (member "--container=main-container" args))
                (should (member "pod/test-pod" args))
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

;; Test kubernetes-logs--get-log-buffers
(ert-deftest kubernetes-logs-test--get-log-buffers ()
  (let* ((test-buffer1 (generate-new-buffer "*kubernetes logs: default/pod/nginx*"))
         (test-buffer2 (generate-new-buffer "*kubernetes logs: production/deployment/api:main*"))
         (non-log-buffer (generate-new-buffer "*some other buffer*"))
         (test-buffers (list test-buffer1 test-buffer2 non-log-buffer)))
    (unwind-protect
        (cl-letf (((symbol-function 'buffer-list) (lambda () test-buffers))
                  ((symbol-function 'derived-mode-p)
                   (lambda (&rest _)
                     ;; Only return t for the kubernetes log buffers
                     (let ((name (buffer-name)))
                       (string-prefix-p "*kubernetes logs: " name)))))
          ;; Test the function
          (let ((log-buffers (kubernetes-logs--get-log-buffers)))
            ;; Should find exactly 2 log buffers
            (should (= (length log-buffers) 2))
            (should (member test-buffer1 log-buffers))
            (should (member test-buffer2 log-buffers))
            (should-not (member non-log-buffer log-buffers))))
      ;; Clean up test buffers
      (dolist (buf test-buffers)
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

;; Test kubernetes-logs-switch-buffers
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
                     (if (functionp collection)
                         ;; Handle the new lambda-based collection function
                         (let ((action (funcall collection "" nil 'metadata)))
                           ;; Verify metadata is correctly set
                           (should (equal action '(metadata (category . buffer))))
                           "*kubernetes logs: default/pod/nginx*")
                       ;; Handle the old style collection
                       (should collection)
                       "*kubernetes logs: default/pod/nginx*")))
                  ((symbol-function 'get-buffer)
                   (lambda (name)
                     (cond
                      ((equal name "*kubernetes logs: default/pod/nginx*") test-buffer1)
                      ((equal name "*kubernetes logs: production/deployment/api:main*") test-buffer2)
                      (t nil))))
                  ((symbol-function 'switch-to-buffer)
                   (lambda (buffer) (setq selected-buffer buffer))))

          ;; Run the function
          (kubernetes-logs-switch-buffers)

          ;; Verify results - first check that the buffer is still alive
          (should (buffer-live-p test-buffer1))
          (should (eq selected-buffer test-buffer1)))

      ;; Clean up test buffers only if they're still alive
      (dolist (buf test-buffers)
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

;; Test display names generated for various log buffer configurations
(ert-deftest kubernetes-logs-test-buffer-display-names ()
  (let* ((test-buffers
          (list
           ;; Pod with no container
           (let ((buf (generate-new-buffer "*kubernetes logs: default/pod/nginx*")))
             (with-current-buffer buf
               (setq-local kubernetes-logs-resource-type "pod")
               (setq-local kubernetes-logs-resource-name "nginx")
               (setq-local kubernetes-logs-namespace "default"))
             buf)
           ;; Pod with container
           (let ((buf (generate-new-buffer "*kubernetes logs: default/pod/nginx:main*")))
             (with-current-buffer buf
               (setq-local kubernetes-logs-resource-type "pod")
               (setq-local kubernetes-logs-resource-name "nginx")
               (setq-local kubernetes-logs-namespace "default")
               (setq-local kubernetes-logs-container-name "main"))
             buf)
           ;; Deployment with container and different namespace
           (let ((buf (generate-new-buffer "*kubernetes logs: production/deployment/api:web*")))
             (with-current-buffer buf
               (setq-local kubernetes-logs-resource-type "deployment")
               (setq-local kubernetes-logs-resource-name "api")
               (setq-local kubernetes-logs-namespace "production")
               (setq-local kubernetes-logs-container-name "web"))
             buf)
           ;; Statefulset with no container
           (let ((buf (generate-new-buffer "*kubernetes logs: default/statefulset/db*")))
             (with-current-buffer buf
               (setq-local kubernetes-logs-resource-type "statefulset")
               (setq-local kubernetes-logs-resource-name "db")
               (setq-local kubernetes-logs-namespace "default"))
             buf)))
         (mock-selected-names '())
         (mock-presented-collections '()))

    (unwind-protect
        (cl-letf (((symbol-function 'buffer-list) (lambda () test-buffers))
                  ((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                  ((symbol-function 'completing-read)
                   (lambda (prompt collection &rest _)
                     ;; Store the collection for testing
                     (when (functionp collection)
                       (let ((test-collection (funcall collection "" nil t)))
                         (setq mock-presented-collections test-collection)))
                     ;; Return a fixed selection to simulate user input
                     (car (if (functionp collection)
                              (setq mock-selected-names (funcall collection "" nil t))
                            collection))))
                  ((symbol-function 'get-buffer) (lambda (name)
                                                   (cl-find-if
                                                    (lambda (buf) (equal (buffer-name buf) name))
                                                    test-buffers)))
                  ((symbol-function 'switch-to-buffer) (lambda (buffer) buffer)))

          ;; Run the function
          (kubernetes-logs-switch-buffers)

          ;; Verify the buffer names are used exactly as-is
          (should (member "*kubernetes logs: default/pod/nginx*" mock-selected-names))
          (should (member "*kubernetes logs: default/pod/nginx:main*" mock-selected-names))
          (should (member "*kubernetes logs: production/deployment/api:web*" mock-selected-names))
          (should (member "*kubernetes logs: default/statefulset/db*" mock-selected-names)))

      ;; Clean up test buffers
      (dolist (buf test-buffers)
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

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
