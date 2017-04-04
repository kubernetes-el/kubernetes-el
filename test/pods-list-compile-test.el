;;; pods-list-compile-test.el --- Test rendering of the pods list  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-and-compile
  (require 'f)

  (defvar project-root
    (locate-dominating-file default-directory ".git"))

  (defvar this-directory
    (f-join project-root "test")))

(require 'kubernetes (f-join project-root "kubernetes.el"))

(defconst sample-get-pods-response
  (let* ((path (f-join this-directory "get-pods-output.json"))
         (sample-response (f-read-text path)))
    (json-read-from-string sample-response)))

(defun draw-pods-section (state)
  (kubernetes--eval-ast (kubernetes--render-pods-section state)))


;; Shows "Fetching..." when state isn't initialized yet.

(defconst drawing-pods-section-loading-result
  (s-trim-left "

Pods
  Name                                          Status     Ready   Restarts    Age
  Fetching...

"))

(ert-deftest drawing-pods-section--empty-state ()
  (with-temp-buffer
    (save-excursion (magit-insert-section (root)
                      (draw-pods-section nil)))
    (should (equal drawing-pods-section-loading-result
                   (substring-no-properties (buffer-string))))
    (forward-line 1)
    (forward-to-indentation)
    (should (equal 'kubernetes-progress-indicator (get-text-property (point) 'face)))))


;; Shows "None" when there are no pods.

(defconst drawing-pods-section-empty-result
  (s-trim-left "

Pods (0)
  None.

"))

(ert-deftest drawing-pods-section--no-pods ()
  (let ((empty-state `((pods . ((items . ,(vector)))))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-pods-section empty-state)))
      (should (equal drawing-pods-section-empty-result
                     (substring-no-properties (buffer-string))))
      (search-forward "None")
      (should (equal 'magit-dimmed (get-text-property (point) 'face))))))


;; Shows pod lines when there are pods.

(defconst drawing-pods-section-sample-result
  (s-trim-left "

Pods (2)
  Name                                          Status     Ready   Restarts    Age
  example-svc-v3-1603416598-2f9lb               Running      1/1        0      36d
    Name:       example-service
    Labels:     example-pod-v3
    Namespace:  ns.example
    Image:      example.com/example-service:3.0.0
    Host IP:    10.0.0.0
    Pod IP:     172.0.0.1
    Started:    2017-02-25T08:12:14Z

  example-svc-v4-1603416598-2f9lb               Running      1/1        0      36d
    Name:       example-service
    Labels:     example-pod-v4
    Namespace:  ns.example
    Image:      example.com/example-service:4.8.0
    Host IP:    10.0.0.0
    Pod IP:     172.0.0.1
    Started:    2017-02-25T08:12:14Z


"))

(ert-deftest drawing-pods-section--sample-response ()
  (let ((state `((pods . ,sample-get-pods-response)
                 (current-time . ,(date-to-time "2017-04-03 00:00Z")))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-pods-section state)))
      (should (equal drawing-pods-section-sample-result
                     (substring-no-properties (buffer-string)))))))

(ert-deftest drawing-pods-section--sample-response-text-properties ()
  (let ((state `((pods . ,sample-get-pods-response))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-pods-section state)))
      ;; Skip past header.
      (forward-line 2)

      (dolist (key '("Name"
                     "Labels"
                     "Namespace"
                     "Image"
                     "Host IP"
                     "Pod IP"
                     "Started"))
        (save-excursion
          (search-forward key)
          (should (equal 'magit-header-line (get-text-property (point) 'face))))))))

;;; pods-list-compile-test.el ends here
