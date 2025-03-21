;;; kubernetes-events-test.el --- Tests for Kubernetes events functionality  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'kubernetes-events)
(require 'kubernetes-utils)

(declare-function test-helper-json-resource "test-helper.el")

(defconst kubernetes-events-test--sample-event
  '((type . "Warning")
    (reason . "PolicyViolation")
    (message . "policy require-run-as-nonroot/autogen-run-as-non-root fail: validation error: Running as root is not allowed.")
    (metadata (creationTimestamp . "2025-03-16T18:50:26Z")
              (name . "test-deployment.182d5d20e4a8654d")
              (namespace . "default"))
    (involvedObject (name . "test-deployment")
                    (namespace . "default")
                    (kind . "Deployment")))
  "Sample event data for testing.")

(defconst kubernetes-events-test--sample-events
  `((items . [,kubernetes-events-test--sample-event
              ((type . "Normal")
               (reason . "Created")
               (message . "Created container nginx")
               (metadata (creationTimestamp . "2025-03-15T12:31:00Z")
                         (name . "test-pod.17f9bdb42d66a81f")
                         (namespace . "default"))
               (involvedObject (name . "test-pod")
                               (namespace . "default")
                               (kind . "Pod")))]))
  "Sample events response for testing.")

(defun kubernetes-events-test--draw-events-buffer (events)
  "Return a buffer with EVENTS drawn in it."
  (let ((buf (get-buffer-create "*test-events*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq major-mode 'kubernetes-events-mode)
        (kubernetes-events--redraw-buffer events "Test Events")))
    buf))

(ert-deftest kubernetes-events-test--format-time ()
  "Test formatting of timestamps."
  (cl-letf (((symbol-function 'kubernetes-events--format-time)
             (lambda (timestamp)
               "02:10:20")))
    (should (equal "02:10:20" (kubernetes-events--format-time "2025-03-16T18:50:26Z")))))

(ert-deftest kubernetes-events-test--calc-time-diff ()
  "Test time difference calculation."
  (cl-letf (((symbol-function 'kubernetes-events--calc-time-diff)
             (lambda (timestamp)
               "18h")))

    (should (equal "18h" (kubernetes-events--calc-time-diff "2025-03-16T00:00:00Z")))))

(ert-deftest kubernetes-events-test--format-type ()
  "Test formatting of event types with appropriate faces."
  (should (eq 'error
              (get-text-property 0 'face (kubernetes-events--format-type "Warning"))))
  (should (eq 'kubernetes-dimmed
              (get-text-property 0 'face (kubernetes-events--format-type "Normal"))))
  (should (eq 'default
              (get-text-property 0 'face (kubernetes-events--format-type "Unknown")))))

(ert-deftest kubernetes-events-test--wrap-text ()
  "Test text wrapping function."
  (let ((long-text "This is a very long text that should be wrapped at the specified width of 20 characters.")
        (short-text "Short text."))
    (should (string-match-p "\n" (kubernetes-events--wrap-text long-text 20)))
    (should (= 1 (length (split-string (kubernetes-events--wrap-text short-text 20) "\n"))))))

(ert-deftest kubernetes-events-test--convert-items-if-needed ()
  "Test vector to list conversion."
  (let ((vector-items [1 2 3])
        (list-items '(1 2 3)))
    (should (listp (kubernetes-events--convert-items-if-needed vector-items)))
    (should (equal '(1 2 3) (kubernetes-events--convert-items-if-needed vector-items)))
    (should (listp (kubernetes-events--convert-items-if-needed list-items)))
    (should (equal '(1 2 3) (kubernetes-events--convert-items-if-needed list-items)))))

(ert-deftest kubernetes-events-test--generate-buffer-name ()
  "Test buffer name generation for events."
  (cl-letf (((symbol-function 'kubernetes-state--get) (lambda (_state key)
                                                       (when (eq key 'current-namespace)
                                                         "test-namespace"))))
    (should (equal "*kubernetes events: test-namespace/pod/nginx*"
                   (kubernetes-events--generate-buffer-name "pod" "nginx")))

    (should (equal "*kubernetes events: test-namespace*"
                   (kubernetes-events--generate-buffer-name nil nil)))))

(ert-deftest kubernetes-events-test--read-resource-if-needed ()
  "Test resource detection at point."
  (cl-letf (((symbol-function 'kubernetes-utils-get-resource-info-at-point)
             (lambda () '("pod" . "test-pod"))))
    (should (equal '("pod" . "test-pod")
                   (kubernetes-events--read-resource-if-needed nil))))

  (cl-letf (((symbol-function 'kubernetes-utils-get-resource-info-at-point)
             (lambda () nil)))
    (should (eq nil (kubernetes-events--read-resource-if-needed nil)))))

(ert-deftest kubernetes-events-test--insert-event ()
  "Test event insertion."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (setq major-mode 'kubernetes-events-mode)

      (kubernetes-events--insert-event kubernetes-events-test--sample-event)

      (let ((content (buffer-string)))
        (should (string-match-p "Warning" content))
        (should (string-match-p "PolicyViolation" content))
        (should (string-match-p "test-deplo" content))
        (should (string-match-p "Deployment" content)))

      (goto-char (point-min))
      (should (get-text-property (point) 'event-data))
      (should (get-text-property (point) 'kubernetes-nav)))))

(ert-deftest kubernetes-events-test--redraw-buffer ()
  "Test redrawing events buffer."
  (let ((buf (kubernetes-events-test--draw-events-buffer kubernetes-events-test--sample-events)))
    (unwind-protect
        (with-current-buffer buf
          (let ((content (buffer-string)))
            (should (string-match-p "Test Events" content))
            (should (string-match-p "Time" content))
            (should (string-match-p "Age" content))
            (should (string-match-p "Type" content))
            (should (string-match-p "Reason" content))
            (should (string-match-p "Object" content))
            (should (string-match-p "Kind" content))
            (should (string-match-p "Message" content))

            (should (string-match-p "Warning" content))
            (should (string-match-p "PolicyViolation" content))
            (should (string-match-p "test-deplo" content))
            (should (string-match-p "Deployment" content))
            (should (string-match-p "Created container" content))
            (should (string-match-p "Pod" content))))

      (kill-buffer buf))))

(ert-deftest kubernetes-events-test--redraw-buffer-empty ()
  "Test redrawing events buffer with no events."
  (let ((buf (kubernetes-events-test--draw-events-buffer nil)))
    (unwind-protect
        (with-current-buffer buf
          (let ((content (buffer-string)))
            (should (string-match-p "Test Events" content))
            (should (string-match-p "No events found" content))))

      (kill-buffer buf))))

(ert-deftest kubernetes-events-test-view-message ()
  "Test viewing event message."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (insert "  ")
      (add-text-properties (point-min) (point-max)
                           (list 'event-data kubernetes-events-test--sample-event))

      (with-current-buffer (get-buffer-create "*Kubernetes Event Message*")
        (erase-buffer)
        (insert "policy require-run-as-nonroot"))

      (cl-letf (((symbol-function 'display-buffer) (lambda (buf) (selected-window)))
                ((symbol-function 'select-window) (lambda (win) nil))
                ((symbol-function 'view-mode) (lambda () nil)))

        (kubernetes-events-view-message)

        (let ((msg-buf (get-buffer "*Kubernetes Event Message*")))
          (should msg-buf))))))

(ert-deftest kubernetes-events-test-inspect-event ()
  "Test inspecting event."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (insert "  ")
      (add-text-properties (point-min) (point-max)
                           (list 'event-data kubernetes-events-test--sample-event))

      (with-current-buffer (get-buffer-create "*Kubernetes Event*")
        (erase-buffer))

      (cl-letf (((symbol-function 'kubernetes-yaml-make-buffer)
                 (lambda (name data)
                   (should (equal name "*Kubernetes Event*"))
                   (should (equal data kubernetes-events-test--sample-event))
                   (get-buffer "*Kubernetes Event*")))
                ((symbol-function 'display-buffer) (lambda (buf) (selected-window)))
                ((symbol-function 'select-window) (lambda (win) nil)))

        (kubernetes-events-inspect-event)

        (should (get-buffer "*Kubernetes Event*"))))))

(ert-deftest kubernetes-events-test-view-resource-at-point ()
  "Test viewing resource at point."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (orig-nav-function (symbol-function 'kubernetes-nav-pop-to-resource-at-point)))
      (insert "  ")
      (add-text-properties (point-min) (point-max)
                           (list 'kubernetes-nav '(:pod-name "test-pod")))

      (unwind-protect
          (progn
            (fset 'kubernetes-nav-pop-to-resource-at-point
                  (lambda () t))

            (kubernetes-events-view-resource-at-point)

            (should t))

        (fset 'kubernetes-nav-pop-to-resource-at-point orig-nav-function)))))

(ert-deftest kubernetes-events-test-quit ()
  "Test quitting events buffer."
  (let ((buf (get-buffer-create "*kubernetes events: test*"))
        (called-with nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'get-buffer-window) (lambda (b) 'mock-window))
                   ((symbol-function 'quit-window) (lambda (kill win) (setq called-with win))))

            (with-current-buffer buf
              (kubernetes-events-quit)

              (should (eq called-with 'mock-window)))))

      (kill-buffer buf))))

(ert-deftest kubernetes-events-test-fetch ()
  "Test fetching events."
  (let ((state (make-hash-table))
        (events-buf1 (get-buffer-create "*kubernetes events: test-namespace*"))
        (events-buf2 (get-buffer-create "*kubernetes events: test-namespace/pod/nginx*"))
        (kubectl-called-args nil))

    (unwind-protect
        (cl-letf (((symbol-function 'kubernetes-state) (lambda () state))
                  ((symbol-function 'kubernetes-state--get) (lambda (_state _key) "test-namespace"))
                  ((symbol-function 'kubernetes-kubectl)
                   (lambda (state args on-success _on-error)
                     (setq kubectl-called-args args)
                     (with-temp-buffer
                       (insert "{\"items\": []}")
                       (funcall on-success (current-buffer)))))
                  ((symbol-function 'display-buffer) (lambda (buf) (selected-window)))
                  ((symbol-function 'select-window) (lambda (win) nil)))

          (kubernetes-events-fetch nil nil nil)
          (should (equal kubectl-called-args '("events" "-o" "json")))

          (kubernetes-events-fetch "pod" "nginx" nil)
          (should (equal kubectl-called-args
                        '("events" "-o" "json" "--for" "Pod/nginx"))))

      (kill-buffer events-buf1)
      (kill-buffer events-buf2))))

(ert-deftest kubernetes-events-test-fetch-all ()
  "Test fetch-all command."
  (let ((state (make-hash-table))
        (resource-at-point nil)
        (fetch-called-with nil))

    (cl-letf (((symbol-function 'kubernetes-state) (lambda () state))
              ((symbol-function 'kubernetes-events--read-resource-if-needed)
               (lambda (_) resource-at-point))
              ((symbol-function 'kubernetes-events-fetch)
               (lambda (type name args)
                 (setq fetch-called-with (list type name args)))))

      (setq resource-at-point '("pod" . "nginx"))
      (kubernetes-events-fetch-all 'test-args state)
      (should (equal '("pod" "nginx" test-args) fetch-called-with))

      (setq resource-at-point nil)
      (kubernetes-events-fetch-all 'test-args state)
      (should (equal '(nil nil test-args) fetch-called-with)))))

(ert-deftest kubernetes-events-test-refresh ()
  "Test refresh command."
  (let ((events-buffer (get-buffer-create "*kubernetes events: test-namespace*"))
        (fetch-called-with nil))
    (unwind-protect
        (cl-letf (((symbol-function 'kubernetes-events-fetch)
                   (lambda (type name args)
                     (setq fetch-called-with (list type name args)))))

          (with-current-buffer events-buffer
            (kubernetes-events-refresh)
            (should (equal '(nil nil nil) fetch-called-with))))

      (kill-buffer events-buffer))))

(ert-deftest kubernetes-events-test-non-json-response ()
  "Test handling of non-JSON response from kubectl."
  (let ((state (make-hash-table))
        (events-buf (get-buffer-create "*kubernetes events: test-namespace*"))
        (kubectl-response nil)
        (buffer-updated nil))

    (unwind-protect
        (cl-letf (((symbol-function 'kubernetes-state) (lambda () state))
                  ((symbol-function 'kubernetes-state--get) (lambda (_state _key) "test-namespace"))
                  ((symbol-function 'kubernetes-kubectl)
                   (lambda (state args on-success _on-error)
                     (with-temp-buffer
                       (insert "No events found in test-namespace namespace.")
                       (funcall on-success (current-buffer)))))
                  ((symbol-function 'kubernetes-events--redraw-buffer)
                   (lambda (events title)
                     (setq buffer-updated t)
                     (should (eq events nil))
                     (should (string= title "All cluster events"))))
                  ((symbol-function 'display-buffer) (lambda (buf) (selected-window)))
                  ((symbol-function 'select-window) (lambda (win) nil)))

          (kubernetes-events-fetch nil nil nil)

          (should buffer-updated))

      (kill-buffer events-buf))))

(ert-deftest kubernetes-events-test-response-handler ()
  "Test the response handler function directly."
  (let ((json-response-buf (get-buffer-create "*test-json-response*"))
        (text-response-buf (get-buffer-create "*test-text-response*"))
        (events-buf (get-buffer-create "*kubernetes events: test-namespace*"))
        (json-handler-called nil)
        (nil-handler-called nil))

    (unwind-protect
        (cl-letf (((symbol-function 'kubernetes-events--redraw-buffer)
                   (lambda (events title)
                     (if events
                         (setq json-handler-called t)
                       (setq nil-handler-called t)))))

          (with-current-buffer json-response-buf
            (erase-buffer)
            (insert "{\"items\": []}"))

          (with-current-buffer text-response-buf
            (erase-buffer)
            (insert "No events found in test-namespace namespace."))

          (with-current-buffer events-buf
            (setq-local buffer-name "*kubernetes events: test-namespace*"))

          (let ((response-handler
                 (lambda (buf)
                   (with-current-buffer buf
                     (let ((content (buffer-string)))
                       (condition-case err
                           (let ((events (json-read-from-string content)))
                             (with-current-buffer events-buf
                               (kubernetes-events--redraw-buffer events "Test")))
                         (json-error
                          (with-current-buffer events-buf
                            (kubernetes-events--redraw-buffer nil "Test")))))))))

            (setq json-handler-called nil)
            (setq nil-handler-called nil)
            (funcall response-handler json-response-buf)
            (should json-handler-called)
            (should-not nil-handler-called)

            (setq json-handler-called nil)
            (setq nil-handler-called nil)
            (funcall response-handler text-response-buf)
            (should-not json-handler-called)
            (should nil-handler-called)))

      (kill-buffer json-response-buf)
      (kill-buffer text-response-buf)
      (kill-buffer events-buf))))

(provide 'kubernetes-events-test)

;;; kubernetes-events-test.el ends here
