;;; kubernetes-pods-test.el --- Test rendering of the pods list  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'kubernetes-pods)
(declare-function test-helper-json-resource "test-helper.el")


(defconst sample-get-pods-response (test-helper-json-resource "get-pods-response.json"))

(defun draw-pods-section (state)
  (kubernetes-ast-eval `(pods-list ,state)))


;; Shows "Fetching..." when state isn't initialized yet.

(defconst kubernetes-pods-test--loading-result
  (s-trim-left "

Pods
  Name                                          Status        Ready   Restarts    Age
  Fetching...

"))

(ert-deftest kubernetes-pods-test--empty-state ()
  (with-temp-buffer
    (save-excursion (magit-insert-section (root)
                      (draw-pods-section nil)))
    (should (equal kubernetes-pods-test--loading-result
                   (substring-no-properties (buffer-string))))
    (forward-line 1)
    (forward-to-indentation)
    (should (equal 'kubernetes-progress-indicator (get-text-property (point) 'face)))))


;; Shows "None" when there are no pods.

(defconst kubernetes-pods-test--empty-result
  (s-trim-left "

Pods (0)
  None.

"))

(ert-deftest kubernetes-pods-test--no-pods ()
  (let ((empty-state `((pods . ((items . ,(vector)))))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-pods-section empty-state)))
      (should (equal kubernetes-pods-test--empty-result
                     (substring-no-properties (buffer-string))))
      (search-forward "None")
      (should (equal 'kubernetes-dimmed (get-text-property (point) 'face))))))


;; Shows pod lines when there are pods.

(defconst kubernetes-pods-test--sample-result-without-completed
  (s-trim-left "

Pods (4)
  Name                                          Status        Ready   Restarts    Age
  example-svc-v3-1603416598-2f9lb               Running         1/1          0    36d
    Label:      example-pod-v3
    Namespace:  ns.example
    Host IP:    10.0.0.0
    Pod IP:     172.0.0.1
    Started:    2017-02-25T08:12:14Z
    Containers: (1)
    - Name:     example-service-2
      Image:    example.com/example-service:3.0.0

  example-svc-v4-1603416598-2f9lb               Running         2/2          0    36d
    Label:      example-pod-v4
    Namespace:  ns.example
    Host IP:    10.0.0.0
    Pod IP:     172.0.0.1
    Started:    2017-02-25T08:12:14Z
    Init Containers: (2)
    - Name:     init-config
      Image:    example.com/config-initializer:1.2.0
    - Name:     init-dependencies
      Image:    example.com/service-checker:2.1.0
    Containers: (2)
    - Name:     example-service-4
      Image:    example.com/example-service:4.8.0
    - Name:     fake-runner-0
      Image:    fake.com/fake-runner:1.0.0

  example-svc-v5-1603416598-2f9lb               Running         0/0          0    36d
    Label:      example-pod-v5
    Namespace:  ns.example
    Host IP:    10.0.0.0
    Pod IP:     172.0.0.1
    Started:    2017-02-25T08:12:14Z
    Containers: (0)


"))

(ert-deftest kubernetes-pods-test--sample-response-without-completed ()
  (let ((state `((pods . ,sample-get-pods-response)
                 (current-time . ,(date-to-time "2017-04-03 00:00Z")))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-pods-section state)))
      (should (equal kubernetes-pods-test--sample-result-without-completed
                     (substring-no-properties (buffer-string)))))))

(defconst kubernetes-pods-test--sample-result-with-completed
  (concat (s-trim-right kubernetes-pods-test--sample-result-without-completed) "

  example-svc-v6-1603416598-2f9lb               Succeeded       0/1          0    36d
    Label:      example-pod-v6
    Namespace:  ns.example
    Host IP:    10.27.111.26
    Pod IP:     172.16.0.216
    Started:    2017-02-25T08:13:39Z
    Containers: (1)
    - Name:     example-svc-v6
      Image:    example.com/example-service:4.8.0


"))

(ert-deftest kubernetes-pods-test--sample-response-with-completed ()
  (let ((state `((pods . ,sample-get-pods-response)
                 (current-time . ,(date-to-time "2017-04-03 00:00Z"))))
        (kubernetes-pods-display-completed t))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-pods-section state)))
      (should (equal kubernetes-pods-test--sample-result-with-completed
                     (substring-no-properties (buffer-string)))))))


(ert-deftest kubernetes-pods-test--sample-response-text-properties ()
  (let ((state `((pods . ,sample-get-pods-response)
                 (current-time . ,(date-to-time "2017-04-03 00:00Z")))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-pods-section state)))
      ;; Skip past header.
      (forward-line 2)

      (dolist (key '("Name"
                     "Label"
                     "Namespace"
                     "Image"
                     "Host IP"
                     "Pod IP"
                     "Started"))
        (save-excursion
          (search-forward key)
          (should (equal 'magit-section-heading (get-text-property (point) 'face))))))))

(ert-deftest kubernetes-pods-test--pod-view-line-pending-pod ()
  "Test that pod-view-line component can handle a Pending pod with nil containerStatuses."
  (with-temp-buffer
    (let* ((current-time (current-time))
           (pending-pod '((metadata . ((name . "pending-test-pod")))
                          (status . ((phase . "Pending")
                                    (startTime . "2023-01-01T00:00:00Z")))))
           (state `((current-time . ,current-time)))
           (rendered (progn
                       ;; Using with-temp-buffer to keep the buffer alive
                       (erase-buffer)
                       (insert " ") ;; Ensure buffer isn't empty
                       (let ((inhibit-read-only t))
                         (kubernetes-ast-eval `(pod-view-line ,state ,pending-pod)))
                       (buffer-string))))
      (should rendered)
      (should (string-match-p "Pending" rendered)))))

(ert-deftest kubernetes-pods-test--pod-view-line-evicted-pod ()
  "Test that pod-view-line component can handle an Evicted pod."
  (with-temp-buffer
    (let* ((current-time (current-time))
           (evicted-pod '((metadata . ((name . "evicted-test-pod")))
                          (status . ((phase . "Failed")
                                    (reason . "Evicted")
                                    (startTime . "2023-01-01T00:00:00Z")))))
           (state `((current-time . ,current-time)))
           (rendered (progn
                       ;; Using with-temp-buffer to keep the buffer alive
                       (erase-buffer)
                       (insert " ") ;; Ensure buffer isn't empty
                       (let ((inhibit-read-only t))
                         (kubernetes-ast-eval `(pod-view-line ,state ,evicted-pod)))
                       (buffer-string))))
      (should rendered)
      (should (string-match-p "Evicted" rendered)))))

(ert-deftest kubernetes-pods-test--pod-view-line-unschedulable-pod ()
  "Test that pod-view-line component can handle a real unschedulable pod from production."
  (with-temp-buffer
    (let* ((current-time (current-time))
           (unschedulable-pod '((metadata . ((name . "vgw-5d887ffb6b-ld6lz")))
                               (status . ((phase . "Pending")
                                          (conditions . [((lastProbeTime)
                                                          (lastTransitionTime . "2025-03-14T03:53:16Z")
                                                          (message . "0/118 nodes are available...")
                                                          (reason . "Unschedulable")
                                                          (status . "False")
                                                          (type . "PodScheduled"))])
                                          (qosClass . "Burstable")))))
           (state `((current-time . ,current-time)))
           (rendered (progn
                       ;; Using with-temp-buffer to keep the buffer alive
                       (erase-buffer)
                       (insert " ") ;; Ensure buffer isn't empty
                       (let ((inhibit-read-only t))
                         (kubernetes-ast-eval `(pod-view-line ,state ,unschedulable-pod)))
                       (buffer-string))))
      (should rendered)
      (should (or (string-match-p "Pending" rendered)
                  (string-match-p "Unschedulable" rendered))))))

;; Test kubernetes-pods--read-name function
(ert-deftest kubernetes-pods-test--read-name ()
  "Test that kubernetes-pods--read-name properly fetches and displays pod names."
  (let ((mock-state '())
        (mock-pods-response '((items . [((metadata . ((name . "pod1"))))
                                        ((metadata . ((name . "pod2"))))
                                        ((metadata . ((name . "pod3"))))])))
        (mock-completing-read-called nil)
        (mock-completing-read-args nil)
        (mock-message-args nil))

    ;; Mock dependencies
    (cl-letf (((symbol-function 'kubernetes-state--get)
               (lambda (state key)
                 (when (eq key 'pods)
                   nil))) ; Return nil to force fetching pods
              ((symbol-function 'kubernetes-kubectl-get)
               (lambda (resource &optional args cb)
                 ;; Verify that kubectl is called to get pods
                 (should (equal resource "pods"))
                 (when cb (funcall cb mock-pods-response))
                 mock-pods-response))
              ((symbol-function 'kubernetes-state-update-pods)
               (lambda (response)
                 ;; Verify pods are updated in state
                 (should (equal response mock-pods-response))))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq mock-message-args (cons format-string args))))
              ((symbol-function 'completing-read)
               (lambda (prompt collection &rest args)
                 (setq mock-completing-read-called t)
                 (setq mock-completing-read-args (list prompt collection args))
                 "pod2")))

      ;; Call the function
      (let ((result (kubernetes-pods--read-name mock-state)))
        ;; Verify the function's behavior
        (should (equal result "pod2"))
        (should mock-completing-read-called)
        (should (equal (car mock-message-args) "Getting pods..."))

        ;; Verify that completing-read was called with the right arguments
        (should (equal (car mock-completing-read-args) "Pod: "))
        (should (equal (sort (cadr mock-completing-read-args) #'string<)
                       '("pod1" "pod2" "pod3")))))))

;; Test kubernetes-pods--read-name with existing pods in state
(ert-deftest kubernetes-pods-test--read-name-with-existing-pods ()
  "Test kubernetes-pods--read-name when pods are already in state."
  (let ((mock-state '())
        (mock-pods-response '((items . [((metadata . ((name . "pod1"))))
                                        ((metadata . ((name . "pod2"))))
                                        ((metadata . ((name . "pod3"))))])))
        (mock-completing-read-called nil)
        (mock-kubectl-called nil))

    ;; Mock dependencies
    (cl-letf (((symbol-function 'kubernetes-state--get)
               (lambda (state key)
                 (when (eq key 'pods)
                   mock-pods-response))) ; Return pods data to avoid fetch
              ((symbol-function 'kubernetes-kubectl-await-on-async)
               (lambda (state fun)
                 (setq mock-kubectl-called t)
                 nil)) ; This shouldn't be called
              ((symbol-function 'completing-read)
               (lambda (prompt collection &rest args)
                 (setq mock-completing-read-called t)
                 "pod3")))

      ;; Call the function
      (let ((result (kubernetes-pods--read-name mock-state)))
        ;; Verify the function's behavior
        (should (equal result "pod3"))
        (should mock-completing-read-called)
        (should-not mock-kubectl-called)))))

;; Test kubernetes-pods-delete-marked function
(ert-deftest kubernetes-pods-test-delete-marked ()
  "Test that kubernetes-pods-delete-marked properly handles marked pods."
  (let ((mock-state '())
        (marked-pods '("pod1" "pod2" "pod3"))
        (deleted-pods '())
        (marked-after-deletion '())
        (redraw-triggered nil))

    ;; Mock dependencies
    (cl-letf (((symbol-function 'kubernetes-state--get)
               (lambda (state key)
                 (when (eq key 'marked-pods)
                   marked-pods)))
              ((symbol-function 'kubernetes-state-delete-pod)
               (lambda (name)
                 (push name deleted-pods)))
              ((symbol-function 'kubernetes-kubectl-delete)
               (lambda (resource-type name state success-callback error-callback)
                 ;; Verify parameters
                 (should (equal resource-type "pod"))
                 (should (member name marked-pods))

                 ;; Simulate success for pod1 and pod2, failure for pod3
                 (if (equal name "pod3")
                     (funcall error-callback nil)
                   (funcall success-callback nil))))
              ((symbol-function 'kubernetes-state-mark-pod)
               (lambda (name)
                 (push name marked-after-deletion)))
              ((symbol-function 'kubernetes-state-trigger-redraw)
               (lambda ()
                 (setq redraw-triggered t)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 nil))) ; Ignore messages

      ;; Call the function
      (kubernetes-pods-delete-marked mock-state)

      ;; Verify behavior
      (should (equal (sort deleted-pods #'string<)
                     (sort marked-pods #'string<)))
      (should (equal marked-after-deletion '("pod3"))) ; Only pod3 should be marked after deletion
      (should redraw-triggered))))

(ert-deftest kubernetes-pods-test-display-pod-success-path ()
  "Test the success path of kubernetes-display-pod function."
  (let ((mock-state '())
        (mock-pod-name "test-pod")
        (mock-pod '((metadata . ((name . "test-pod")))))
        (yaml-buffer-created nil)
        (buffer-displayed nil)
        (window-selected nil))

    ;; Mock all dependencies
    (cl-letf (((symbol-function 'kubernetes-state-lookup-pod)
               (lambda (name state)
                 (should (equal name mock-pod-name))
                 (should (eq state mock-state))
                 mock-pod))
              ((symbol-function 'kubernetes-yaml-make-buffer)
               (lambda (buffer-name pod)
                 (setq yaml-buffer-created t)
                 (should (equal buffer-name kubernetes-pod-buffer-name))
                 (should (equal pod mock-pod))
                 (get-buffer-create "*k8s-test-buffer*")))
              ((symbol-function 'display-buffer)
               (lambda (buffer)
                 (setq buffer-displayed t)
                 (should (equal (buffer-name buffer) "*k8s-test-buffer*"))
                 (selected-window)))
              ((symbol-function 'select-window)
               (lambda (window)
                 (setq window-selected t))))

      ;; Execute the function we're testing
      (kubernetes-display-pod mock-pod-name mock-state)

      ;; Verify the function executed the full path correctly
      (should yaml-buffer-created)
      (should buffer-displayed)
      (should window-selected)

      ;; Cleanup
      (when (get-buffer "*k8s-test-buffer*")
        (kill-buffer "*k8s-test-buffer*")))))

(ert-deftest kubernetes-pods-test-display-pod-error-path ()
  "Test the error path of kubernetes-display-pod when pod is not found."
  (let ((mock-state '())
        (mock-pod-name "nonexistent-pod")
        (error-message-received nil))

    ;; Mock the pod lookup to return nil (pod not found)
    (cl-letf (((symbol-function 'kubernetes-state-lookup-pod)
               (lambda (name state)
                 (should (equal name mock-pod-name))
                 (should (eq state mock-state))
                 nil)) ; Return nil to indicate pod not found
              ((symbol-function 'error)
               (lambda (format-string &rest args)
                 (setq error-message-received
                       (apply #'format format-string args))
                 (signal 'error nil))))

      ;; Function should throw an error
      (should-error (kubernetes-display-pod mock-pod-name mock-state))

      ;; Verify error message
      (should (equal error-message-received
                     (format "Unknown pod: %s" mock-pod-name))))))

(ert-deftest kubernetes-pods-test-display-pod-interactive-spec ()
  "Test the interactive specification of kubernetes-display-pod."
  (let* ((mock-state '((pods . ((items . [((metadata . ((name . "test-pod"))))])))))
         (read-pod-name "test-pod")
         (state-called nil))

    ;; Mock dependencies
    (cl-letf (((symbol-function 'kubernetes-state)
               (lambda ()
                 (setq state-called t)
                 mock-state))
              ((symbol-function 'kubernetes-pods--read-name)
               (lambda (state)
                 (should (eq state mock-state))
                 read-pod-name))
              ;; Mock the actual function to avoid side effects
              ((symbol-function 'kubernetes-state-lookup-pod)
               (lambda (name state) '((metadata . ((name . "test-pod"))))))
              ((symbol-function 'kubernetes-yaml-make-buffer)
               (lambda (buffer-name pod) (generate-new-buffer "*mock-buffer*")))
              ((symbol-function 'display-buffer) #'identity)
              ((symbol-function 'select-window) #'identity))

      ;; Get the interactive spec
      (let* ((interactive-spec (interactive-form 'kubernetes-display-pod))
             (interactive-fun (cadr interactive-spec)))

        ;; Evaluate the interactive form to get the arguments
        (let ((args (eval interactive-fun)))
          ;; Verify the correct arguments were produced
          (should (equal (car args) read-pod-name))
          (should (eq (cadr args) mock-state))
          (should state-called)))

      ;; Clean up
      (when (get-buffer "*mock-buffer*")
        (kill-buffer "*mock-buffer*")))))

(ert-deftest kubernetes-pods-test-display-unkown-pod ()
  "Test the error path of kubernetes-display-pod with basic approach."
  (let ((pod-name "nonexistent-pod")
        (state '())
        (error-message nil))

    ;; Only mock the lookup function to return nil
    (cl-letf (((symbol-function 'kubernetes-state-lookup-pod)
               (lambda (name state) nil)))

      ;; Capture the error message
      (condition-case err
          (kubernetes-display-pod pod-name state)
        (error
         (setq error-message (cadr err))))

      ;; Verify the error message
      (should (equal error-message (format "Unknown pod: %s" pod-name))))))

;;; kubernetes-pods-test.el ends here
