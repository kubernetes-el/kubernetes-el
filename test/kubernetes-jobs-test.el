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
      (should (equal 'magit-dimmed (get-text-property (point) 'face))))))


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
                 (current-time . ,(date-to-time "2017-05-03 00:00Z")))))
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
      Running     example-svc-v3-1603416598-2f9lb

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
                 (current-time . ,(date-to-time "2017-05-03 00:00Z"))
                 (pods . ,sample-get-pods-response))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-jobs-section state)))
      (should (equal kubernetes-jobs-test--sample-result--pod-exists
                     (substring-no-properties (buffer-string)))))))


;;; kubernetes-jobs-test.el ends here
