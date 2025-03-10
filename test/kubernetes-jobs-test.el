;;; kubernetes-jobs-test.el --- Test rendering of the jobs list  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'kubernetes-jobs)
(declare-function test-helper-json-resource "test-helper.el")

(defconst sample-get-pods-response (test-helper-json-resource "get-pods-response.json"))

(defconst sample-get-jobs-response (test-helper-json-resource "get-jobs-response.json"))

(defun draw-jobs-section (state)
  (kubernetes-ast-eval `(jobs-list ,state)))


;; Shows "Fetching..." when state isn't initialized yet.

(defconst kubernetes-jobs-test--loading-result
  (s-trim-left "

Jobs
  Name                                          Successful    Age
  Fetching...

"))

(ert-deftest kubernetes-jobs-test--empty-state ()
  (with-temp-buffer
    (save-excursion (magit-insert-section (root)
                      (draw-jobs-section nil)))
    (should (equal kubernetes-jobs-test--loading-result
                   (substring-no-properties (buffer-string))))
    (forward-line 1)
    (forward-to-indentation)
    (should (equal 'kubernetes-progress-indicator (get-text-property (point) 'face)))))


;; Shows "None" when there are no jobs.

(defconst kubernetes-jobs-test--empty-result
  (s-trim-left "

Jobs (0)
  None.

"))

(ert-deftest kubernetes-jobs-test--no-jobs ()
  (let ((empty-state `((jobs . ((items . ,(vector)))))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-jobs-section empty-state)))
      (should (equal kubernetes-jobs-test--empty-result
                     (substring-no-properties (buffer-string))))
      (search-forward "None")
      (should (equal 'kubernetes-dimmed (get-text-property (point) 'face))))))


;; Shows job lines when there are jobs.

(defconst kubernetes-jobs-test--sample-result--pods-loading
  (s-trim-left "

Jobs (2)
  Name                                          Successful    Age
  example-job-1                                          1    10d
    Namespace:  mm
    RestartPolicy: OnFailure

    Created:    2017-04-22T22:00:02Z
    Started:    2017-04-22T22:00:03Z
    Completed:  2017-04-23T00:00:05Z

    Pod
      Fetching...

  example-job-2                                          1    10d
    Namespace:  mm
    RestartPolicy: OnFailure

    Created:    2017-04-22T22:00:02Z
    Started:    2017-04-22T22:00:03Z
    Completed:  2017-04-23T00:00:05Z

    Pod
      Fetching...


"))

(ert-deftest kubernetes-jobs-test--sample-response--pods-loading ()
  (let ((state `((jobs . ,sample-get-jobs-response)
                 (current-time . ,(date-to-time "2017-05-03T00:00Z")))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-jobs-section state)))
      (should (equal kubernetes-jobs-test--sample-result--pods-loading
                     (substring-no-properties (buffer-string)))))))


;; Show entries when pods are found in the application state.

(defconst kubernetes-jobs-test--sample-result--pod-exists
  (s-trim-left "

Jobs (2)
  Name                                          Successful    Age
  example-job-1                                          1    10d
    Namespace:  mm
    RestartPolicy: OnFailure

    Created:    2017-04-22T22:00:02Z
    Started:    2017-04-22T22:00:03Z
    Completed:  2017-04-23T00:00:05Z

    Pod
      Succeeded   example-svc-v6-1603416598-2f9lb

  example-job-2                                          1    10d
    Namespace:  mm
    RestartPolicy: OnFailure

    Created:    2017-04-22T22:00:02Z
    Started:    2017-04-22T22:00:03Z
    Completed:  2017-04-23T00:00:05Z

    Pod
      Not found.

"))

(ert-deftest kubernetes-jobs-test--sample-response--pod-exists ()
  (let ((state `((jobs . ,sample-get-jobs-response)
                 (current-time . ,(date-to-time "2017-05-03T00:00Z"))
                 (pods . ,sample-get-pods-response))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-jobs-section state)))
      (should (equal kubernetes-jobs-test--sample-result--pod-exists
                     (substring-no-properties (buffer-string)))))))


(ert-deftest kubernetes-jobs-delete-marked-test ()
  "Test deleting marked jobs."
  (let* ((test-state (make-hash-table :test 'equal))
         (deleted-jobs '())
         (state-updated nil))

    ;; Mock functions
    (cl-letf (((symbol-function 'kubernetes-state--get)
               (lambda (_state key)
                 (when (eq key 'marked-jobs)
                   '("example-job-1" "example-job-2"))))
              ((symbol-function 'kubernetes-state-delete-job)
               (lambda (name) (push name deleted-jobs)))
              ((symbol-function 'kubernetes-kubectl-delete)
               (lambda (_type name _state success-fn _error-fn)
                 (funcall success-fn nil)))
              ((symbol-function 'kubernetes-state-trigger-redraw)
               (lambda () (setq state-updated t))))

      ;; Execute function
      (kubernetes-jobs-delete-marked test-state)

      ;; Verify results
      (should (equal (sort deleted-jobs #'string<)
                     (sort '("example-job-1" "example-job-2") #'string<)))
      (should state-updated))))

(ert-deftest kubernetes-jobs-delete-marked-failure-test ()
  "Test handling of failed job deletion."
  (let* ((test-state (make-hash-table :test 'equal))
         (remarked-jobs '()))

    ;; Mock functions
    (cl-letf (((symbol-function 'kubernetes-state--get)
               (lambda (_state key)
                 (when (eq key 'marked-jobs)
                   '("example-job-1"))))
              ((symbol-function 'kubernetes-state-delete-job)
               (lambda (_name) t))
              ((symbol-function 'kubernetes-kubectl-delete)
               (lambda (_type _name _state _success-fn error-fn)
                 (funcall error-fn nil)))
              ((symbol-function 'kubernetes-state-mark-job)
               (lambda (name) (push name remarked-jobs))))

      ;; Execute function
      (kubernetes-jobs-delete-marked test-state)

      ;; Verify job was remarked after failure
      (should (equal remarked-jobs '("example-job-1"))))))

;; Tests for kubernetes-jobs--read-name

(ert-deftest kubernetes-jobs--read-name-test ()
  "Test reading job name with existing state."
  (let* ((test-state (make-hash-table :test 'equal))
         (completing-read-called nil)
         (completing-read-choices nil))

    ;; Mock functions
    (cl-letf (((symbol-function 'kubernetes-state--get)
               (lambda (_state _key) sample-get-jobs-response))
              ((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _)
                 (setq completing-read-called t
                       completing-read-choices choices)
                 "example-job-1")))

      ;; Execute function
      (let ((result (kubernetes-jobs--read-name test-state)))

      ;; Verify results
      (should completing-read-called)
      (should (equal result "example-job-1"))
      ;; Verify choices include both jobs from fixture
      (should (equal (sort completing-read-choices #'string<)
                     '("example-job-1" "example-job-2")))))))

(ert-deftest kubernetes-jobs--read-name-fetch-test ()
  "Test reading job name when state needs to be fetched."
  (let* ((test-state (make-hash-table :test 'equal))
         (state-updated nil))

    ;; Mock functions
    (cl-letf (((symbol-function 'kubernetes-state--get)
               (lambda (_state _key) nil))
              ((symbol-function 'kubernetes-kubectl-await-on-async)
               (lambda (_state _fn) sample-get-jobs-response))
              ((symbol-function 'kubernetes-state-update-jobs)
               (lambda (_) (setq state-updated t)))
              ((symbol-function 'completing-read)
               (lambda (_prompt _choices &rest _) "example-job-1")))

      ;; Execute function
      (let ((result (kubernetes-jobs--read-name test-state)))

      ;; Verify state was updated and name was returned
      (should state-updated)
      (should (equal result "example-job-1"))))))

;; Tests for kubernetes-display-job

(ert-deftest kubernetes-display-job-test ()
  "Test displaying job information."
  (let* ((test-state (make-hash-table :test 'equal))
         (displayed-job nil)
         (buffer-displayed nil))

    ;; Mock functions
    (cl-letf (((symbol-function 'kubernetes-state-lookup-job)
               (lambda (name _state)
                 (let* ((items (alist-get 'items sample-get-jobs-response))
                        (job (seq-find (lambda (j)
                                       (equal name (alist-get 'name (alist-get 'metadata j))))
                                     items)))
                   (setq displayed-job job)
                   job)))
              ((symbol-function 'kubernetes-yaml-make-buffer)
               (lambda (_name _job) (generate-new-buffer "test-buffer")))
              ((symbol-function 'display-buffer)
               (lambda (_buffer) (selected-window)))
              ((symbol-function 'select-window)
               (lambda (_) (setq buffer-displayed t))))

      ;; Execute function
      (kubernetes-display-job "example-job-1" test-state)

      ;; Verify buffer was displayed with correct job
      (should buffer-displayed)
      (should displayed-job)
      (should (equal (alist-get 'name (alist-get 'metadata displayed-job))
                     "example-job-1")))))

(ert-deftest kubernetes-display-job-error-test ()
  "Test error handling when displaying unknown job."
  (let ((test-state (make-hash-table :test 'equal)))

    ;; Mock functions
    (cl-letf (((symbol-function 'kubernetes-state-lookup-job)
               (lambda (_name _state) nil)))

      ;; Should signal error for unknown job
      (should-error (kubernetes-display-job "unknown-job" test-state)
                    :type 'error))))

;;; kubernetes-jobs-test.el ends here
