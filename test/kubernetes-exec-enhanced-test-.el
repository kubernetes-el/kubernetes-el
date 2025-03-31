;;; kubernetes-exec-enhanced-test.el --- Enhanced tests for kubernetes-exec.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; This file provides additional test coverage for kubernetes-exec.el
;;; Code:

(require 's)
(require 'cl-lib)
(require 'kubernetes-exec)
(require 'kubernetes-utils)

;; Test kubernetes-exec-into with comprehensive input combinations
(ert-deftest kubernetes-exec-test-exec-into-comprehensive ()
  "Comprehensive test of kubernetes-exec-into with various input combinations."
  (let ((commands-executed nil)
        (buffers-created nil))
    (cl-letf (((symbol-function 'kubernetes-kubectl--flags-from-state)
               (lambda (_) '("--kubeconfig=/test/config")))
              ((symbol-function 'kubernetes-state--get)
               (lambda (_ key) (when (eq key 'current-namespace) "test-namespace")))
              ((symbol-function 'kubernetes-utils-term-buffer-start)
               (lambda (buffer-name executable args)
                 ;; Store command for verification
                 (push (list 'term buffer-name executable args) commands-executed)
                 ;; Return a dummy buffer
                 (let ((buf (get-buffer-create buffer-name)))
                   (push buf buffers-created)
                   buf)))
              ((symbol-function 'kubernetes-utils-process-buffer-start)
               (lambda (buffer-name mode executable args)
                 ;; Store command for verification
                 (push (list 'process buffer-name executable args) commands-executed)
                 ;; Return a dummy buffer
                 (let ((buf (get-buffer-create buffer-name)))
                   (push buf buffers-created)
                   buf)))
              ((symbol-function 'get-buffer-process) (lambda (_) nil))
              ((symbol-function 'set-process-sentinel) #'ignore)
              ((symbol-function 'select-window) #'ignore)
              ((symbol-function 'display-buffer) #'identity))

      (unwind-protect
          (progn
            ;; Test 1: Basic pod with no TTY (should use process-buffer)
            (kubernetes-exec-into "test-pod" '() "/bin/bash" 'mock-state)

            ;; Test 2: Pod with TTY flag (should use term-buffer)
            (kubernetes-exec-into "test-pod" '("--tty") "/bin/bash" 'mock-state)

            ;; Test 3: Pod with both TTY and stdin (common case)
            (kubernetes-exec-into "test-pod" '("--tty" "--stdin") "/bin/bash" 'mock-state)

            ;; Test 4: Pod with container specified
            (kubernetes-exec-into "test-pod" '("--container=main-container") "/bin/bash" 'mock-state)

            ;; Test 5: Deployment with TTY and container
            (kubernetes-exec-into "deployment/test-deployment"
                                 '("--tty" "--container=web")
                                 "/bin/bash"
                                 'mock-state)

            ;; Test 6: StatefulSet with TTY
            (kubernetes-exec-into "statefulset/db-statefulset"
                                 '("--tty")
                                 "/bin/bash"
                                 'mock-state)

            ;; Test 7: Resource in non-default namespace
            (cl-letf (((symbol-function 'kubernetes-state--get)
                       (lambda (_ key) (when (eq key 'current-namespace) "production"))))
              (kubernetes-exec-into "job/worker-job"
                                   '("--tty")
                                   "/bin/bash"
                                   'mock-state))

            ;; Test 8: Different command
            (kubernetes-exec-into "pod/test-pod"
                                 '("--tty")
                                 "/bin/sh"
                                 'mock-state)

            ;; Now verify each test case
            ;; Commands are in reverse order, so start from the end
            (let* ((commands (nreverse commands-executed))
                   (test1 (nth 0 commands))
                   (test2 (nth 1 commands))
                   (test3 (nth 2 commands))
                   (test4 (nth 3 commands))
                   (test5 (nth 4 commands))
                   (test6 (nth 5 commands))
                   (test7 (nth 6 commands))
                   (test8 (nth 7 commands)))

              ;; Test 1: Basic pod with no TTY (should use process-buffer)
              (should (eq (car test1) 'process))
              (should (string-match-p "pod/test-pod" (nth 1 test1)))
              (should (member "pod/test-pod" (nth 3 test1)))
              (should (member "/bin/bash" (nth 3 test1)))
              (should-not (member "--tty" (nth 3 test1)))

              ;; Test 2: Pod with TTY flag (should use term-buffer)
              (should (eq (car test2) 'term))
              (should (string-match-p "pod/test-pod" (nth 1 test2)))
              (should (member "pod/test-pod" (nth 3 test2)))
              (should (member "--tty" (nth 3 test2)))

              ;; Test 3: Pod with both TTY and stdin
              (should (eq (car test3) 'term))
              (should (string-match-p "pod/test-pod" (nth 1 test3)))
              (should (member "--tty" (nth 3 test3)))
              (should (member "--stdin" (nth 3 test3)))

              ;; Test 4: Pod with container specified
              (should (eq (car test4) 'process))
              (should (string-match-p "main-container" (nth 1 test4)))
              (should (member "--container=main-container" (nth 3 test4)))

              ;; Test 5: Deployment with TTY and container
              (should (eq (car test5) 'term))
              (should (string-match-p "deployment/test-deployment:web" (nth 1 test5)))
              (should (member "deployment/test-deployment" (nth 3 test5)))
              (should (member "--container=web" (nth 3 test5)))

              ;; Test 6: StatefulSet with TTY
              (should (eq (car test6) 'term))
              (should (string-match-p "statefulset/db-statefulset" (nth 1 test6)))
              (should (member "statefulset/db-statefulset" (nth 3 test6)))

              ;; Test 7: Resource in non-default namespace
              (should (eq (car test7) 'term))
              (should (string-match-p "production/job/worker-job" (nth 1 test7)))
              (should (member "job/worker-job" (nth 3 test7)))
              (should (member "--namespace=production" (nth 3 test7)))

              ;; Test 8: Different command
              (should (eq (car test8) 'term))
              (should (string-match-p "pod/test-pod" (nth 1 test8)))
              (should (member "/bin/sh" (nth 3 test8)))))

        ;; Clean up test buffers
        (dolist (buf buffers-created)
          (when (buffer-live-p buf)
            (kill-buffer buf)))))))

;; Test kubernetes-exec-into with edge cases and errors
(ert-deftest kubernetes-exec-test-exec-into-edge-cases ()
  "Test kubernetes-exec-into with edge cases and error handling."
  (let ((commands-executed nil)
        (buffers-created nil))
    (cl-letf (((symbol-function 'kubernetes-kubectl--flags-from-state)
               (lambda (_) '("--kubeconfig=/test/config")))
              ((symbol-function 'kubernetes-state--get)
               (lambda (_ key) (when (eq key 'current-namespace) "test-namespace")))
              ((symbol-function 'kubernetes-utils-term-buffer-start)
               (lambda (buffer-name executable args)
                 ;; Store command for verification
                 (push (list 'term buffer-name executable args) commands-executed)
                 ;; Return a dummy buffer
                 (let ((buf (get-buffer-create buffer-name)))
                   (push buf buffers-created)
                   buf)))
              ((symbol-function 'kubernetes-utils-process-buffer-start)
               (lambda (buffer-name mode executable args)
                 ;; Store command for verification
                 (push (list 'process buffer-name executable args) commands-executed)
                 ;; Return a dummy buffer
                 (let ((buf (get-buffer-create buffer-name)))
                   (push buf buffers-created)
                   buf)))
              ((symbol-function 'get-buffer-process) (lambda (_) nil))
              ((symbol-function 'set-process-sentinel) #'ignore)
              ((symbol-function 'select-window) #'ignore)
              ((symbol-function 'display-buffer) #'identity))

      (unwind-protect
          (progn
            ;; Test 1: Empty command (should use default)
            (cl-letf (((symbol-function 'string-empty-p)
                       (lambda (s) t)))
              (kubernetes-exec-into "test-pod" '() "" 'mock-state))

            ;; Test 2: Resource name with special characters
            (kubernetes-exec-into "pod/test-pod-with-special@chars.123"
                                 '("--tty")
                                 "/bin/bash"
                                 'mock-state)

            ;; Test 3: Resource name with namespace included in format
            (kubernetes-exec-into "namespace/test-namespace/pod/test-pod"
                                 '("--tty")
                                 "/bin/bash"
                                 'mock-state)

            ;; Test 4: Multiple containers with specific selection
            (kubernetes-exec-into "pod/multi-container-pod"
                                 '("--tty" "--container=app" "--container=sidecar")
                                 "/bin/bash"
                                 'mock-state)

            ;; Verify results
            (let* ((commands (nreverse commands-executed))
                   (test1 (nth 0 commands))
                   (test2 (nth 1 commands))
                   (test3 (nth 2 commands))
                   (test4 (nth 3 commands)))

              ;; Test 1: Empty command (should use default)
              (should (member kubernetes-default-exec-command (nth 3 test1)))

              ;; Test 2: Resource name with special characters
              (should (string-match-p "test-pod-with-special@chars.123" (nth 1 test2)))
              (should (member "pod/test-pod-with-special@chars.123" (nth 3 test2)))

              ;; Test 3: Resource name with namespace format (should be parsed)
              ;; This test verifies that complex resource paths are handled properly
              (should (member "pod/test-pod" (nth 3 test3)))

              ;; Test 4: Multiple containers (last one wins)
              (should (string-match-p "container=sidecar" (buffer-name (car buffers-created))))
              (should (member "--container=app" (nth 3 test4)))
              (should (member "--container=sidecar" (nth 3 test4)))))

        ;; Clean up test buffers
        (dolist (buf buffers-created)
          (when (buffer-live-p buf)
            (kill-buffer buf)))))))

;; Test kubernetes-exec-using-vterm with mocked vterm module
(ert-deftest kubernetes-exec-test-using-vterm-with-mock ()
  "Test kubernetes-exec-using-vterm with mocked vterm module."
  (let ((vterm-called nil)
        (vterm-buffer-name nil)
        (vterm-shell nil)
        (require-called nil)
        (require-feature nil))

    ;; Setup mocks for vterm functionality
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional noerror)
                 (setq require-called t)
                 (setq require-feature feature)
                 (when (eq feature 'vterm)
                   (if noerror
                       nil
                     t))))
              ((symbol-function 'vterm-other-window)
               (lambda ()
                 (setq vterm-called t)
                 (get-buffer-create vterm-buffer-name)))
              ((symbol-function 'kubernetes-state--get)
               (lambda (_ key) (when (eq key 'current-namespace) "test-namespace")))
              ((symbol-function 'kubernetes-kubectl--flags-from-state)
               (lambda (_) '("--kubeconfig=/test/config"))))

      ;; Test 1: Basic vterm execution with pod
      (let ((vterm-buffer-name "*kubernetes exec vterm: test-namespace/pod/test-pod*")
            (vterm-shell "kubectl exec --kubeconfig=/test/config test-pod -- /bin/bash"))
        (setq vterm-called nil)
        (kubernetes-exec-using-vterm "test-pod" '() "/bin/bash" 'mock-state)
        (should require-called)
        (should (eq require-feature 'vterm))
        (should vterm-called)
        (should (string-match-p "test-pod -- /bin/bash" vterm-shell)))

      ;; Test 2: Vterm with deployment
      (let ((vterm-buffer-name "*kubernetes exec vterm: test-namespace/deployment/web-app*")
            (vterm-shell "kubectl exec --kubeconfig=/test/config deployment/web-app -- /bin/bash"))
        (setq vterm-called nil)
        (kubernetes-exec-using-vterm "deployment/web-app" '() "/bin/bash" 'mock-state)
        (should vterm-called)
        (should (string-match-p "deployment/web-app -- /bin/bash" vterm-shell)))

      ;; Test 3: Vterm with container specified
      (let ((vterm-buffer-name "*kubernetes exec vterm: test-namespace/pod/test-pod:web*")
            (vterm-shell "kubectl exec --kubeconfig=/test/config --container=web pod/test-pod -- /bin/bash"))
        (setq vterm-called nil)
        (kubernetes-exec-using-vterm "pod/test-pod" '("--container=web") "/bin/bash" 'mock-state)
        (should vterm-called)
        (should (string-match-p "--container=web" vterm-shell)))

      ;; Test 4: Vterm with TTY and stdin flags
      (let ((vterm-buffer-name "*kubernetes exec vterm: test-namespace/pod/test-pod*")
            (vterm-shell "kubectl exec --kubeconfig=/test/config --tty --stdin pod/test-pod -- /bin/bash"))
        (setq vterm-called nil)
        (kubernetes-exec-using-vterm "pod/test-pod" '("--tty" "--stdin") "/bin/bash" 'mock-state)
        (should vterm-called)
        (should (string-match-p "--tty --stdin" vterm-shell))))))

;; Test kubernetes-exec-using-vterm with vterm module not available
(ert-deftest kubernetes-exec-test-using-vterm-not-available ()
  "Test kubernetes-exec-using-vterm when vterm module is not available."
  (cl-letf (((symbol-function 'require)
             (lambda (feature &optional noerror)
               (when (eq feature 'vterm)
                 (if noerror
                     nil
                   (signal 'file-missing `("Cannot open load file" "No such file" ,feature)))))))

    ;; Should throw error when vterm is not available
    (should-error (kubernetes-exec-using-vterm "test-pod" '() "/bin/bash" 'mock-state)
                 :type 'error)))

;; Test kubernetes-exec-into-with-check with different scenarios
(ert-deftest kubernetes-exec-test-into-with-check-comprehensive ()
  "Comprehensive test for kubernetes-exec-into-with-check function."
  (let ((kubernetes-utils--selected-resource nil)
        (exec-into-args nil)
        (error-raised nil))

    ;; Setup mocks for testing
    (cl-letf (((symbol-function 'kubernetes-utils-has-valid-resource-p)
               (lambda (types) t))  ;; Start with valid resource
              ((symbol-function 'kubernetes-state)
               (lambda () 'mock-state))
              ((symbol-function 'transient-args)
               (lambda (_) '("--tty" "--stdin")))
              ((symbol-function 'kubernetes-utils-get-effective-resource)
               (lambda (state types) (cons "pod" "test-pod")))
              ((symbol-function 'read-string)
               (lambda (prompt &optional initial history default)
                 (should (string-match-p "Command" prompt))
                 "/bin/custom-shell"))
              ((symbol-function 'string-empty-p)
               (lambda (s) (equal s "")))
              ((symbol-function 'string-trim)
               (lambda (s) s))
              ((symbol-function 'kubernetes-exec-into)
               (lambda (resource-path args command state)
                 (setq exec-into-args (list resource-path args command state))
                 "success")))

      ;; Test 1: Normal execution with custom command
      (setq exec-into-args nil)
      (let ((result (kubernetes-exec-into-with-check '("--tty" "--stdin") 'mock-state)))
        (should (equal result "success"))
        (should (equal (nth 0 exec-into-args) "test-pod"))
        (should (equal (nth 1 exec-into-args) '("--tty" "--stdin")))
        (should (equal (nth 2 exec-into-args) "/bin/custom-shell")))

      ;; Test 2: With deployment resource
      (setq exec-into-args nil)
      (cl-letf (((symbol-function 'kubernetes-utils-get-effective-resource)
                 (lambda (state types) (cons "deployment" "web-app"))))
        (kubernetes-exec-into-with-check '("--tty") 'mock-state)
        (should (equal (nth 0 exec-into-args) "deployment/web-app"))
        (should (equal (nth 1 exec-into-args) '("--tty"))))

      ;; Test 3: With empty command (should use default)
      (setq exec-into-args nil)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &optional initial history default) ""))
                ((symbol-function 'string-empty-p)
                 (lambda (s) t)))
        (kubernetes-exec-into-with-check '("--tty") 'mock-state)
        (should (equal (nth 2 exec-into-args) kubernetes-default-exec-command)))

      ;; Test 4: No valid resource
      (setq error-raised nil)
      (cl-letf (((symbol-function 'kubernetes-utils-has-valid-resource-p)
                 (lambda (types) nil))
                ((symbol-function 'user-error)
                 (lambda (msg &rest args)
                   (setq error-raised t)
                   (should (string-match-p "No resource selected" msg))
                   (signal 'user-error (list msg)))))
        (should-error (kubernetes-exec-into-with-check '("--tty") 'mock-state)
                     :type 'user-error)
        (should error-raised)))))

;; Test kubernetes-exec-vterm-with-check functionality
(ert-deftest kubernetes-exec-test-vterm-with-check-comprehensive ()
  "Comprehensive test for kubernetes-exec-vterm-with-check function."
  (let ((kubernetes-utils--selected-resource nil)
        (exec-vterm-args nil)
        (error-raised nil)
        (vterm-available t))

    ;; Setup mocks for testing
    (cl-letf (((symbol-function 'kubernetes-utils-has-valid-resource-p)
               (lambda (types) t))  ;; Start with valid resource
              ((symbol-function 'kubernetes-state)
               (lambda () 'mock-state))
              ((symbol-function 'transient-args)
               (lambda (_) '("--tty" "--stdin")))
              ((symbol-function 'kubernetes-utils-get-effective-resource)
               (lambda (state types) (cons "pod" "test-pod")))
              ((symbol-function 'read-string)
               (lambda (prompt &optional initial history default)
                 (should (string-match-p "Command" prompt))
                 "/bin/custom-shell"))
              ((symbol-function 'string-empty-p)
               (lambda (s) (equal s "")))
              ((symbol-function 'string-trim)
               (lambda (s) s))
              ((symbol-function 'require)
               (lambda (feature &optional noerror)
                 (when (eq feature 'vterm)
                   vterm-available)))
              ((symbol-function 'kubernetes-exec-using-vterm)
               (lambda (resource-path args command state)
                 (setq exec-vterm-args (list resource-path args command state))
                 "vterm-success")))

      ;; Test 1: Normal execution with custom command
      (setq exec-vterm-args nil)
      (let ((result (kubernetes-exec-vterm-with-check '("--tty" "--stdin") 'mock-state)))
        (should (equal result "vterm-success"))
        (should (equal (nth 0 exec-vterm-args) "test-pod"))
        (should (equal (nth 1 exec-vterm-args) '("--tty" "--stdin")))
        (should (equal (nth 2 exec-vterm-args) "/bin/custom-shell")))

      ;; Test 2: With deployment resource
      (setq exec-vterm-args nil)
      (cl-letf (((symbol-function 'kubernetes-utils-get-effective-resource)
                 (lambda (state types) (cons "deployment" "web-app"))))
        (kubernetes-exec-vterm-with-check '("--tty") 'mock-state)
        (should (equal (nth 0 exec-vterm-args) "deployment/web-app"))
        (should (equal (nth 1 exec-vterm-args) '("--tty"))))

      ;; Test 3: With empty command (should use default)
      (setq exec-vterm-args nil)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &optional initial history default) ""))
                ((symbol-function 'string-empty-p)
                 (lambda (s) t)))
        (kubernetes-exec-vterm-with-check '("--tty") 'mock-state)
        (should (equal (nth 2 exec-vterm-args) kubernetes-default-exec-command)))

      ;; Test 4: No valid resource
      (setq error-raised nil)
      (cl-letf (((symbol-function 'kubernetes-utils-has-valid-resource-p)
                 (lambda (types) nil))
                ((symbol-function 'user-error)
                 (lambda (msg &rest args)
                   (setq error-raised t)
                   (should (string-match-p "No resource selected" msg))
                   (signal 'user-error (list msg)))))
        (should-error (kubernetes-exec-vterm-with-check '("--tty") 'mock-state)
                     :type 'user-error)
        (should error-raised))

      ;; Test 5: vterm not available
      (setq vterm-available nil)
      (setq error-raised nil)
      (cl-letf (((symbol-function 'user-error)
                 (lambda (msg &rest args)
                   (setq error-raised t)
                   (signal 'user-error (list msg)))))
        (should-error (kubernetes-exec-vterm-with-check '("--tty") 'mock-state)
                     :type 'error)))))

;; Test buffer cleanup functionality
(ert-deftest kubernetes-exec-test-buffer-cleanup ()
  "Test kubernetes-exec buffer cleanup functionality."
  (let ((kubernetes-clean-up-interactive-exec-buffers t)
        (process-sentinel-called nil)
        (test-buffer (get-buffer-create "*kubernetes exec term: test-namespace/pod/test-pod*")))

    (unwind-protect
        (cl-letf (((symbol-function 'kubernetes-kubectl--flags-from-state)
                   (lambda (_) '("--kubeconfig=/test/config")))
                  ((symbol-function 'kubernetes-state--get)
                   (lambda (_ key) (when (eq key 'current-namespace) "test-namespace")))
                  ((symbol-function 'kubernetes-utils-term-buffer-start)
                   (lambda (buffer-name executable args)
                     test-buffer))
                  ((symbol-function 'get-buffer-process)
                   (lambda (_) 'mock-process))
                  ((symbol-function 'set-process-sentinel)
                   (lambda (proc sentinel)
                     (setq process-sentinel-called t)
                     (should (eq proc 'mock-process))
                     (should (eq sentinel #'kubernetes-process-kill-quietly))))
                  ((symbol-function 'select-window) #'ignore)
                  ((symbol-function 'display-buffer) #'identity))

          ;; Test with interactive TTY (should set process sentinel)
          (setq process-sentinel-called nil)
          (kubernetes-exec--exec-internal "pod" "test-pod" '("--tty") "/bin/bash" 'mock-state)
          (should process-sentinel-called)

          ;; Test with cleanup disabled
          (setq process-sentinel-called nil)
          (let ((kubernetes-clean-up-interactive-exec-buffers nil))
            (kubernetes-exec--exec-internal "pod" "test-pod" '("--tty") "/bin/bash" 'mock-state)
            (should-not process-sentinel-called)))

      ;; Clean up test buffer
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer)))))

;; Test exec-switch-buffers with multiple buffers
(ert-deftest kubernetes-exec-test-switch-buffers-multiple ()
  "Test kubernetes-exec-switch-buffers with multiple exec buffers."
  (let* ((test-buffer1 (generate-new-buffer "*kubernetes exec term: default/pod/nginx*"))
         (test-buffer2 (generate-new-buffer "*kubernetes exec term: production/deployment/api:main*"))
         (test-buffer3 (generate-new-buffer "*kubernetes exec vterm: staging/job/backup*"))
         (test-buffers (list test-buffer1 test-buffer2 test-buffer3))
         (selected-buffer nil))

    (unwind-protect
        (cl-letf (((symbol-function 'buffer-list)
                   (lambda () (append test-buffers (list (get-buffer-create "*scratch*")))))
                  ((symbol-function 'completing-read)
                   (lambda (prompt collection &rest _)
                     (if (functionp collection)
                         ;; Test the metadata behavior
                         (let ((action (funcall collection "" nil 'metadata)))
                           (should (equal action '(metadata (category . buffer))))
                           ;; Return expected results for different inputs
                           (let ((all-completions (funcall collection "" nil t)))
                             (should (= (length all-completions) 3))
                             (should (member "*kubernetes exec term: default/pod/nginx*" all-completions))
                             (should (member "*kubernetes exec term: production/deployment/api:main*" all-completions))
                             (should (member "*kubernetes exec vterm: staging/job/backup*" all-completions))
                             "*kubernetes exec vterm: staging/job/backup*"))
                       ;; Handle the old style collection
                       "*kubernetes exec vterm: staging/job/backup*")))
                  ((symbol-function 'get-buffer)
                   (lambda (name)
                     (cond
                      ((equal name "*kubernetes exec term: default/pod/nginx*") test-buffer1)
                      ((equal name "*kubernetes exec term: production/deployment/api:main*") test-buffer2)
                      ((equal name "*kubernetes exec vterm: staging/job/backup*") test-buffer3)
                      (t nil))))
                  ((symbol-function 'switch-to-buffer)
                   (lambda (buffer) (setq selected-buffer buffer))))

          ;; Run the function
          (kubernetes-exec-switch-buffers)

          ;; Verify results - should select the buffer we returned in completing-read
          (should (buffer-live-p test-buffer3))
          (should (eq selected-buffer test-buffer3)))

      ;; Clean up test buffers
      (dolist (buf test-buffers)
        (when (buffer-live-p buf)
          (kill-buffer buf))))))
;;; kubernetes-exec-enhanced-test.el --- Enhanced tests for kubernetes-exec.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; This file provides additional test coverage for kubernetes-exec.el
;;; focusing particularly on the following functions:
;;;  - kubernetes-exec-into
;;;  - kubernetes-exec-using-vterm
;;;  - kubernetes-exec-into-with-check
;;; Code:

(require 's)
(require 'cl-lib)
(require 'kubernetes-exec)
(require 'kubernetes-utils)

;; Test kubernetes-exec-into with comprehensive input combinations
(ert-deftest kubernetes-exec-test-exec-into-comprehensive ()
  "Comprehensive test of kubernetes-exec-into with various input combinations."
  (let ((commands-executed nil)
        (buffers-created nil))
    (cl-letf (((symbol-function 'kubernetes-kubectl--flags-from-state)
               (lambda (_) '("--kubeconfig=/test/config")))
              ((symbol-function 'kubernetes-state--get)
               (lambda (_ key) (when (eq key 'current-namespace) "test-namespace")))
              ((symbol-function 'kubernetes-utils-term-buffer-start)
               (lambda (buffer-name executable args)
                 ;; Store command for verification
                 (push (list 'term buffer-name executable args) commands-executed)
                 ;; Return a dummy buffer
                 (let ((buf (get-buffer-create buffer-name)))
                   (push buf buffers-created)
                   buf)))
              ((symbol-function 'kubernetes-utils-process-buffer-start)
               (lambda (buffer-name mode executable args)
                 ;; Store command for verification
                 (push (list 'process buffer-name executable args) commands-executed)
                 ;; Return a dummy buffer
                 (let ((buf (get-buffer-create buffer-name)))
                   (push buf buffers-created)
                   buf)))
              ((symbol-function 'get-buffer-process) (lambda (_) nil))
              ((symbol-function 'set-process-sentinel) #'ignore)
              ((symbol-function 'select-window) #'ignore)
              ((symbol-function 'display-buffer) #'identity))

      (unwind-protect
          (progn
            ;; Test 1: Basic pod with no TTY (should use process-buffer)
            (kubernetes-exec-into "test-pod" '() "/bin/bash" 'mock-state)

            ;; Test 2: Pod with TTY flag (should use term-buffer)
            (kubernetes-exec-into "test-pod" '("--tty") "/bin/bash" 'mock-state)

            ;; Test 3: Pod with both TTY and stdin (common case)
            (kubernetes-exec-into "test-pod" '("--tty" "--stdin") "/bin/bash" 'mock-state)

            ;; Test 4: Pod with container specified
            (kubernetes-exec-into "test-pod" '("--container=main-container") "/bin/bash" 'mock-state)

            ;; Test 5: Deployment with TTY and container
            (kubernetes-exec-into "deployment/test-deployment"
                                 '("--tty" "--container=web")
                                 "/bin/bash"
                                 'mock-state)

            ;; Test 6: StatefulSet with TTY
            (kubernetes-exec-into "statefulset/db-statefulset"
                                 '("--tty")
                                 "/bin/bash"
                                 'mock-state)

            ;; Test 7: Resource in non-default namespace
            (cl-letf (((symbol-function 'kubernetes-state--get)
                       (lambda (_ key) (when (eq key 'current-namespace) "production"))))
              (kubernetes-exec-into "job/worker-job"
                                   '("--tty")
                                   "/bin/bash"
                                   'mock-state))

            ;; Test 8: Different command
            (kubernetes-exec-into "pod/test-pod"
                                 '("--tty")
                                 "/bin/sh"
                                 'mock-state)

            ;; Now verify each test case
            ;; Commands are in reverse order, so start from the end
            (let* ((commands (nreverse commands-executed))
                   (test1 (nth 0 commands))
                   (test2 (nth 1 commands))
                   (test3 (nth 2 commands))
                   (test4 (nth 3 commands))
                   (test5 (nth 4 commands))
                   (test6 (nth 5 commands))
                   (test7 (nth 6 commands))
                   (test8 (nth 7 commands)))

              ;; Test 1: Basic pod with no TTY (should use process-buffer)
              (should (eq (car test1) 'process))
              (should (string-match-p "pod/test-pod" (nth 1 test1)))
              (should (member "pod/test-pod" (nth 3 test1)))
              (should (member "/bin/bash" (nth 3 test1)))
              (should-not (member "--tty" (nth 3 test1)))

              ;; Test 2: Pod with TTY flag (should use term-buffer)
              (should (eq (car test2) 'term))
              (should (string-match-p "pod/test-pod" (nth 1 test2)))
              (should (member "pod/test-pod" (nth 3 test2)))
              (should (member "--tty" (nth 3 test2)))

              ;; Test 3: Pod with both TTY and stdin
              (should (eq (car test3) 'term))
              (should (string-match-p "pod/test-pod" (nth 1 test3)))
              (should (member "--tty" (nth 3 test3)))
              (should (member "--stdin" (nth 3 test3)))

              ;; Test 4: Pod with container specified
              (should (eq (car test4) 'process))
              (should (string-match-p "main-container" (nth 1 test4)))
              (should (member "--container=main-container" (nth 3 test4)))

              ;; Test 5: Deployment with TTY and container
              (should (eq (car test5) 'term))
              (should (string-match-p "deployment/test-deployment:web" (nth 1 test5)))
              (should (member "deployment/test-deployment" (nth 3 test5)))
              (should (member "--container=web" (nth 3 test5)))

              ;; Test 6: StatefulSet with TTY
              (should (eq (car test6) 'term))
              (should (string-match-p "statefulset/db-statefulset" (nth 1 test6)))
              (should (member "statefulset/db-statefulset" (nth 3 test6)))

              ;; Test 7: Resource in non-default namespace
              (should (eq (car test7) 'term))
              (should (string-match-p "production/job/worker-job" (nth 1 test7)))
              (should (member "job/worker-job" (nth 3 test7)))
              (should (member "--namespace=production" (nth 3 test7)))

              ;; Test 8: Different command
              (should (eq (car test8) 'term))
              (should (string-match-p "pod/test-pod" (nth 1 test8)))
              (should (member "/bin/sh" (nth 3 test8)))))

        ;; Clean up test buffers
        (dolist (buf buffers-created)
          (when (buffer-live-p buf)
            (kill-buffer buf)))))))

;; Test kubernetes-exec-into with edge cases and errors
(ert-deftest kubernetes-exec-test-exec-into-edge-cases ()
  "Test kubernetes-exec-into with edge cases and error handling."
  (let ((commands-executed nil)
        (buffers-created nil))
    (cl-letf (((symbol-function 'kubernetes-kubectl--flags-from-state)
               (lambda (_) '("--kubeconfig=/test/config")))
              ((symbol-function 'kubernetes-state--get)
               (lambda (_ key) (when (eq key 'current-namespace) "test-namespace")))
              ((symbol-function 'kubernetes-utils-term-buffer-start)
               (lambda (buffer-name executable args)
                 ;; Store command for verification
                 (push (list 'term buffer-name executable args) commands-executed)
                 ;; Return a dummy buffer
                 (let ((buf (get-buffer-create buffer-name)))
                   (push buf buffers-created)
                   buf)))
              ((symbol-function 'kubernetes-utils-process-buffer-start)
               (lambda (buffer-name mode executable args)
                 ;; Store command for verification
                 (push (list 'process buffer-name executable args) commands-executed)
                 ;; Return a dummy buffer
                 (let ((buf (get-buffer-create buffer-name)))
                   (push buf buffers-created)
                   buf)))
              ((symbol-function 'get-buffer-process) (lambda (_) nil))
              ((symbol-function 'set-process-sentinel) #'ignore)
              ((symbol-function 'select-window) #'ignore)
              ((symbol-function 'display-buffer) #'identity))

      (unwind-protect
          (progn
            ;; Test 1: Empty command (should use default)
            (cl-letf (((symbol-function 'string-empty-p)
                       (lambda (s) t)))
              (kubernetes-exec-into "test-pod" '() "" 'mock-state))

            ;; Test 2: Resource name with special characters
            (kubernetes-exec-into "pod/test-pod-with-special@chars.123"
                                 '("--tty")
                                 "/bin/bash"
                                 'mock-state)

            ;; Test 3: Resource name with namespace included in format
            (kubernetes-exec-into "namespace/test-namespace/pod/test-pod"
                                 '("--tty")
                                 "/bin/bash"
                                 'mock-state)

            ;; Test 4: Multiple containers with specific selection
            (kubernetes-exec-into "pod/multi-container-pod"
                                 '("--tty" "--container=app" "--container=sidecar")
                                 "/bin/bash"
                                 'mock-state)

            ;; Verify results
            (let* ((commands (nreverse commands-executed))
                   (test1 (nth 0 commands))
                   (test2 (nth 1 commands))
                   (test3 (nth 2 commands))
                   (test4 (nth 3 commands)))

              ;; Test 1: Empty command (should use default)
              (should (member kubernetes-default-exec-command (nth 3 test1)))

              ;; Test 2: Resource name with special characters
              (should (string-match-p "test-pod-with-special@chars.123" (nth 1 test2)))
              (should (member "pod/test-pod-with-special@chars.123" (nth 3 test2)))

              ;; Test 3: Resource name with namespace format (should be parsed)
              ;; This test verifies that complex resource paths are handled properly
              (should (member "pod/test-pod" (nth 3 test3)))

              ;; Test 4: Multiple containers (last one wins)
              (should (string-match-p "container=sidecar" (buffer-name (car buffers-created))))
              (should (member "--container=app" (nth 3 test4)))
              (should (member "--container=sidecar" (nth 3 test4)))))

        ;; Clean up test buffers
        (dolist (buf buffers-created)
          (when (buffer-live-p buf)
            (kill-buffer buf)))))))

;; Test kubernetes-exec-using-vterm with mocked vterm module
(ert-deftest kubernetes-exec-test-using-vterm-with-mock ()
  "Test kubernetes-exec-using-vterm with mocked vterm module."
  (let ((vterm-called nil)
        (vterm-buffer-name nil)
        (vterm-shell nil)
        (require-called nil)
        (require-feature nil))

    ;; Setup mocks for vterm functionality
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional noerror)
                 (setq require-called t)
                 (setq require-feature feature)
                 (when (eq feature 'vterm)
                   (if noerror
                       nil
                     t))))
              ((symbol-function 'vterm-other-window)
               (lambda ()
                 (setq vterm-called t)
                 (get-buffer-create vterm-buffer-name)))
              ((symbol-function 'kubernetes-state--get)
               (lambda (_ key) (when (eq key 'current-namespace) "test-namespace")))
              ((symbol-function 'kubernetes-kubectl--flags-from-state)
               (lambda (_) '("--kubeconfig=/test/config"))))

      ;; Test 1: Basic vterm execution with pod
      (let ((vterm-buffer-name "*kubernetes exec vterm: test-namespace/pod/test-pod*")
            (vterm-shell "kubectl exec --kubeconfig=/test/config test-pod -- /bin/bash"))
        (setq vterm-called nil)
        (kubernetes-exec-using-vterm "test-pod" '() "/bin/bash" 'mock-state)
        (should require-called)
        (should (eq require-feature 'vterm))
        (should vterm-called)
        (should (string-match-p "test-pod -- /bin/bash" vterm-shell)))

      ;; Test 2: Vterm with deployment
      (let ((vterm-buffer-name "*kubernetes exec vterm: test-namespace/deployment/web-app*")
            (vterm-shell "kubectl exec --kubeconfig=/test/config deployment/web-app -- /bin/bash"))
        (setq vterm-called nil)
        (kubernetes-exec-using-vterm "deployment/web-app" '() "/bin/bash" 'mock-state)
        (should vterm-called)
        (should (string-match-p "deployment/web-app -- /bin/bash" vterm-shell)))

      ;; Test 3: Vterm with container specified
      (let ((vterm-buffer-name "*kubernetes exec vterm: test-namespace/pod/test-pod:web*")
            (vterm-shell "kubectl exec --kubeconfig=/test/config --container=web pod/test-pod -- /bin/bash"))
        (setq vterm-called nil)
        (kubernetes-exec-using-vterm "pod/test-pod" '("--container=web") "/bin/bash" 'mock-state)
        (should vterm-called)
        (should (string-match-p "--container=web" vterm-shell)))

      ;; Test 4: Vterm with TTY and stdin flags
      (let ((vterm-buffer-name "*kubernetes exec vterm: test-namespace/pod/test-pod*")
            (vterm-shell "kubectl exec --kubeconfig=/test/config --tty --stdin pod/test-pod -- /bin/bash"))
        (setq vterm-called nil)
        (kubernetes-exec-using-vterm "pod/test-pod" '("--tty" "--stdin") "/bin/bash" 'mock-state)
        (should vterm-called)
        (should (string-match-p "--tty --stdin" vterm-shell))))))

;; Test kubernetes-exec-using-vterm with vterm module not available
(ert-deftest kubernetes-exec-test-using-vterm-not-available ()
  "Test kubernetes-exec-using-vterm when vterm module is not available."
  (cl-letf (((symbol-function 'require)
             (lambda (feature &optional noerror)
               (when (eq feature 'vterm)
                 (if noerror
                     nil
                   (signal 'file-missing `("Cannot open load file" "No such file" ,feature)))))))

    ;; Should throw error when vterm is not available
    (should-error (kubernetes-exec-using-vterm "test-pod" '() "/bin/bash" 'mock-state)
                 :type 'error)))

;; Test kubernetes-exec-into-with-check with different scenarios
(ert-deftest kubernetes-exec-test-into-with-check-comprehensive ()
  "Comprehensive test for kubernetes-exec-into-with-check function."
  (let ((kubernetes-utils--selected-resource nil)
        (exec-into-args nil)
        (error-raised nil))

    ;; Setup mocks for testing
    (cl-letf (((symbol-function 'kubernetes-utils-has-valid-resource-p)
               (lambda (types) t))  ;; Start with valid resource
              ((symbol-function 'kubernetes-state)
               (lambda () 'mock-state))
              ((symbol-function 'transient-args)
               (lambda (_) '("--tty" "--stdin")))
              ((symbol-function 'kubernetes-utils-get-effective-resource)
               (lambda (state types) (cons "pod" "test-pod")))
              ((symbol-function 'read-string)
               (lambda (prompt &optional initial history default)
                 (should (string-match-p "Command" prompt))
                 "/bin/custom-shell"))
              ((symbol-function 'string-empty-p)
               (lambda (s) (equal s "")))
              ((symbol-function 'string-trim)
               (lambda (s) s))
              ((symbol-function 'kubernetes-exec-into)
               (lambda (resource-path args command state)
                 (setq exec-into-args (list resource-path args command state))
                 "success")))

      ;; Test 1: Normal execution with custom command
      (setq exec-into-args nil)
      (let ((result (kubernetes-exec-into-with-check '("--tty" "--stdin") 'mock-state)))
        (should (equal result "success"))
        (should (equal (nth 0 exec-into-args) "test-pod"))
        (should (equal (nth 1 exec-into-args) '("--tty" "--stdin")))
        (should (equal (nth 2 exec-into-args) "/bin/custom-shell")))

      ;; Test 2: With deployment resource
      (setq exec-into-args nil)
      (cl-letf (((symbol-function 'kubernetes-utils-get-effective-resource)
                 (lambda (state types) (cons "deployment" "web-app"))))
        (kubernetes-exec-into-with-check '("--tty") 'mock-state)
        (should (equal (nth 0 exec-into-args) "deployment/web-app"))
        (should (equal (nth 1 exec-into-args) '("--tty"))))

      ;; Test 3: With empty command (should use default)
      (setq exec-into-args nil)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &optional initial history default) ""))
                ((symbol-function 'string-empty-p)
                 (lambda (s) t)))
        (kubernetes-exec-into-with-check '("--tty") 'mock-state)
        (should (equal (nth 2 exec-into-args) kubernetes-default-exec-command)))

      ;; Test 4: No valid resource
      (setq error-raised nil)
      (cl-letf (((symbol-function 'kubernetes-utils-has-valid-resource-p)
                 (lambda (types) nil))
                ((symbol-function 'user-error)
                 (lambda (msg &rest args)
                   (setq error-raised t)
                   (should (string-match-p "No resource selected" msg))
                   (signal 'user-error (list msg)))))
        (should-error (kubernetes-exec-into-with-check '("--tty") 'mock-state)
                     :type 'user-error)
        (should error-raised)))))

;; Test kubernetes-exec-vterm-with-check functionality
(ert-deftest kubernetes-exec-test-vterm-with-check-comprehensive ()
  "Comprehensive test for kubernetes-exec-vterm-with-check function."
  (let ((kubernetes-utils--selected-resource nil)
        (exec-vterm-args nil)
        (error-raised nil)
        (vterm-available t))

    ;; Setup mocks for testing
    (cl-letf (((symbol-function 'kubernetes-utils-has-valid-resource-p)
               (lambda (types) t))  ;; Start with valid resource
              ((symbol-function 'kubernetes-state)
               (lambda () 'mock-state))
              ((symbol-function 'transient-args)
               (lambda (_) '("--tty" "--stdin")))
              ((symbol-function 'kubernetes-utils-get-effective-resource)
               (lambda (state types) (cons "pod" "test-pod")))
              ((symbol-function 'read-string)
               (lambda (prompt &optional initial history default)
                 (should (string-match-p "Command" prompt))
                 "/bin/custom-shell"))
              ((symbol-function 'string-empty-p)
               (lambda (s) (equal s "")))
              ((symbol-function 'string-trim)
               (lambda (s) s))
              ((symbol-function 'require)
               (lambda (feature &optional noerror)
                 (when (eq feature 'vterm)
                   vterm-available)))
              ((symbol-function 'kubernetes-exec-using-vterm)
               (lambda (resource-path args command state)
                 (setq exec-vterm-args (list resource-path args command state))
                 "vterm-success")))

      ;; Test 1: Normal execution with custom command
      (setq exec-vterm-args nil)
      (let ((result (kubernetes-exec-vterm-with-check '("--tty" "--stdin") 'mock-state)))
        (should (equal result "vterm-success"))
        (should (equal (nth 0 exec-vterm-args) "test-pod"))
        (should (equal (nth 1 exec-vterm-args) '("--tty" "--stdin")))
        (should (equal (nth 2 exec-vterm-args) "/bin/custom-shell")))

      ;; Test 2: With deployment resource
      (setq exec-vterm-args nil)
      (cl-letf (((symbol-function 'kubernetes-utils-get-effective-resource)
                 (lambda (state types) (cons "deployment" "web-app"))))
        (kubernetes-exec-vterm-with-check '("--tty") 'mock-state)
        (should (equal (nth 0 exec-vterm-args) "deployment/web-app"))
        (should (equal (nth 1 exec-vterm-args) '("--tty"))))

      ;; Test 3: With empty command (should use default)
      (setq exec-vterm-args nil)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &optional initial history default) ""))
                ((symbol-function 'string-empty-p)
                 (lambda (s) t)))
        (kubernetes-exec-vterm-with-check '("--tty") 'mock-state)
        (should (equal (nth 2 exec-vterm-args) kubernetes-default-exec-command)))

      ;; Test 4: No valid resource
      (setq error-raised nil)
      (cl-letf (((symbol-function 'kubernetes-utils-has-valid-resource-p)
                 (lambda (types) nil))
                ((symbol-function 'user-error)
                 (lambda (msg &rest args)
                   (setq error-raised t)
                   (should (string-match-p "No resource selected" msg))
                   (signal 'user-error (list msg)))))
        (should-error (kubernetes-exec-vterm-with-check '("--tty") 'mock-state)
                     :type 'user-error)
        (should error-raised))

      ;; Test 5: vterm not available
      (setq vterm-available nil)
      (setq error-raised nil)
      (cl-letf (((symbol-function 'user-error)
                 (lambda (msg &rest args)
                   (setq error-raised t)
                   (signal 'user-error (list msg)))))
        (should-error (kubernetes-exec-vterm-with-check '("--tty") 'mock-state)
                     :type 'error)))))

;; Test buffer cleanup functionality
(ert-deftest kubernetes-exec-test-buffer-cleanup ()
  "Test kubernetes-exec buffer cleanup functionality."
  (let ((kubernetes-clean-up-interactive-exec-buffers t)
        (process-sentinel-called nil)
        (test-buffer (get-buffer-create "*kubernetes exec term: test-namespace/pod/test-pod*")))

    (unwind-protect
        (cl-letf (((symbol-function 'kubernetes-kubectl--flags-from-state)
                   (lambda (_) '("--kubeconfig=/test/config")))
                  ((symbol-function 'kubernetes-state--get)
                   (lambda (_ key) (when (eq key 'current-namespace) "test-namespace")))
                  ((symbol-function 'kubernetes-utils-term-buffer-start)
                   (lambda (buffer-name executable args)
                     test-buffer))
                  ((symbol-function 'get-buffer-process)
                   (lambda (_) 'mock-process))
                  ((symbol-function 'set-process-sentinel)
                   (lambda (proc sentinel)
                     (setq process-sentinel-called t)
                     (should (eq proc 'mock-process))
                     (should (eq sentinel #'kubernetes-process-kill-quietly))))
                  ((symbol-function 'select-window) #'ignore)
                  ((symbol-function 'display-buffer) #'identity))

          ;; Test with interactive TTY (should set process sentinel)
          (setq process-sentinel-called nil)
          (kubernetes-exec--exec-internal "pod" "test-pod" '("--tty") "/bin/bash" 'mock-state)
          (should process-sentinel-called)

          ;; Test with cleanup disabled
          (setq process-sentinel-called nil)
          (let ((kubernetes-clean-up-interactive-exec-buffers nil))
            (kubernetes-exec--exec-internal "pod" "test-pod" '("--tty") "/bin/bash" 'mock-state)
            (should-not process-sentinel-called))

          ;; Test buffer-local variables are set correctly
          (with-current-buffer test-buffer
            (should (equal kubernetes-exec-resource-type "pod"))
            (should (equal kubernetes-exec-resource-name "test-pod"))
            (should (equal kubernetes-exec-command "/bin/bash"))
            (should (equal kubernetes-exec-namespace "test-namespace"))
            (should (equal kubernetes-exec-container-name nil))))

      ;; Clean up test buffer
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer)))))

;; Test exec-switch-buffers with multiple buffers
(ert-deftest kubernetes-exec-test-switch-buffers-multiple ()
  "Test kubernetes-exec-switch-buffers with multiple exec buffers."
  (let* ((test-buffer1 (generate-new-buffer "*kubernetes exec term: default/pod/nginx*"))
         (test-buffer2 (generate-new-buffer "*kubernetes exec term: production/deployment/api:main*"))
         (test-buffer3 (generate-new-buffer "*kubernetes exec vterm: staging/job/backup*"))
         (test-buffers (list test-buffer1 test-buffer2 test-buffer3))
         (selected-buffer nil))

    (unwind-protect
        (cl-letf (((symbol-function 'buffer-list)
                   (lambda () (append test-buffers (list (get-buffer-create "*scratch*")))))
                  ((symbol-function 'completing-read)
                   (lambda (prompt collection &rest _)
                     (if (functionp collection)
                         ;; Test the metadata behavior
                         (let ((action (funcall collection "" nil 'metadata)))
                           (should (equal action '(metadata (category . buffer))))
                           ;; Return expected results for different inputs
                           (let ((all-completions (funcall collection "" nil t)))
                             (should (= (length all-completions) 3))
                             (should (member "*kubernetes exec term: default/pod/nginx*" all-completions))
                             (should (member "*kubernetes exec term: production/deployment/api:main*" all-completions))
                             (should (member "*kubernetes exec vterm: staging/job/backup*" all-completions))
                             "*kubernetes exec vterm: staging/job/backup*"))
                       ;; Handle the old style collection
                       "*kubernetes exec vterm: staging/job/backup*")))
                  ((symbol-function 'get-buffer)
                   (lambda (name)
                     (cond
                      ((equal name "*kubernetes exec term: default/pod/nginx*") test-buffer1)
                      ((equal name "*kubernetes exec term: production/deployment/api:main*") test-buffer2)
                      ((equal name "*kubernetes exec vterm: staging/job/backup*") test-buffer3)
                      (t nil))))
                  ((symbol-function 'switch-to-buffer)
                   (lambda (buffer) (setq selected-buffer buffer))))

          ;; Run the function
          (kubernetes-exec-switch-buffers)

          ;; Verify results - should select the buffer we returned in completing-read
          (should (buffer-live-p test-buffer3))
          (should (eq selected-buffer test-buffer3)))

      ;; Clean up test buffers
      (dolist (buf test-buffers)
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

;; Test resource selection and valid resource check logic
(ert-deftest kubernetes-exec-test-resource-selection-logic ()
  "Test the resource selection and validity check logic."
  (let ((kubernetes-utils--selected-resource nil))

    ;; Test has-valid-resource-p with different combinations
    (cl-letf (((symbol-function 'kubernetes-utils-get-resource-at-point)
               (lambda (types) nil)))
      ;; No resource at point, no selected resource
      (should-not (kubernetes-utils-has-valid-resource-p kubernetes-exec-supported-resource-types))

      ;; No resource at point, but selected resource
      (setq kubernetes-utils--selected-resource '("pod" . "test-pod"))
      (should (kubernetes-utils-has-valid-resource-p kubernetes-exec-supported-resource-types))

      ;; Resource at point (will be found first)
      (cl-letf (((symbol-function 'kubernetes-utils-get-resource-at-point)
                 (lambda (types) '("deployment" . "web-app"))))
        (should (kubernetes-utils-has-valid-resource-p kubernetes-exec-supported-resource-types))))

    ;; Test get-effective-resource function
    (setq kubernetes-utils--selected-resource nil)

    ;; Case 1: No resource at point, no selected resource - should prompt and store
    (cl-letf (((symbol-function 'kubernetes-utils-get-resource-at-point)
               (lambda (types) nil))
              ((symbol-function 'kubernetes-utils-select-resource)
               (lambda (state types)
                 (should (equal types kubernetes-exec-supported-resource-types))
                 '("pod" . "selected-pod"))))
      (let ((result (kubernetes-utils-get-effective-resource 'mock-state kubernetes-exec-supported-resource-types)))
        (should (equal result '("pod" . "selected-pod")))
        (should (equal kubernetes-utils--selected-resource '("pod" . "selected-pod")))))

    ;; Case 2: Resource at point, no selected resource - should prefer point
    (setq kubernetes-utils--selected-resource nil)
    (cl-letf (((symbol-function 'kubernetes-utils-get-resource-at-point)
               (lambda (types) '("deployment" . "point-deployment")))
              ((symbol-function 'kubernetes-utils-select-resource)
               (lambda (state types)
                 (error "Should not be called"))))
      (let ((result (kubernetes-utils-get-effective-resource 'mock-state kubernetes-exec-supported-resource-types)))
        (should (equal result '("deployment" . "point-deployment")))
        ;; Should not store when found at point
        (should-not kubernetes-utils--selected-resource)))

    ;; Case 3: No resource at point, but has selected resource - should use selected
    (setq kubernetes-utils--selected-resource '("statefulset" . "stored-statefulset"))
    (cl-letf (((symbol-function 'kubernetes-utils-get-resource-at-point)
               (lambda (types) nil))
              ((symbol-function 'kubernetes-utils-select-resource)
               (lambda (state types)
                 (error "Should not be called"))))
      (let ((result (kubernetes-utils-get-effective-resource 'mock-state kubernetes-exec-supported-resource-types)))
        (should (equal result '("statefulset" . "stored-statefulset")))))

    ;; Test get-current-resource-description function

    ;; Case 1: With resource at point
    (setq kubernetes-utils--selected-resource nil)
    (cl-letf (((symbol-function 'kubernetes-utils-get-resource-at-point)
               (lambda (types) '("pod" . "test-pod"))))
      (let ((desc (kubernetes-utils-get-current-resource-description kubernetes-exec-supported-resource-types)))
        (should (equal desc "pod/test-pod"))))

    ;; Case 2: With selected resource (no resource at point)
    (setq kubernetes-utils--selected-resource '("job" . "backup-job"))
    (cl-letf (((symbol-function 'kubernetes-utils-get-resource-at-point)
               (lambda (types) nil)))
      (let ((desc (kubernetes-utils-get-current-resource-description kubernetes-exec-supported-resource-types)))
        (should (equal desc "job/backup-job"))))

    ;; Case 3: No resource anywhere
    (setq kubernetes-utils--selected-resource nil)
    (cl-letf (((symbol-function 'kubernetes-utils-get-resource-at-point)
               (lambda (types) nil)))
      (let ((desc (kubernetes-utils-get-current-resource-description kubernetes-exec-supported-resource-types)))
        (should (equal desc "selected resource")))))))
