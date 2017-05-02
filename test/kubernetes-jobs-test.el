;;; kubernetes-jobs-test.el --- Test rendering of the jobs list  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'kubernetes-jobs)
(declare-function test-helper-json-resource "test-helper.el")


(defconst sample-get-jobs-response (test-helper-json-resource "get-jobs-response.json"))

(defun draw-jobs-section (state)
  (kubernetes-ast-eval (kubernetes-jobs-render state)))


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

(defconst kubernetes-jobs-test--sample-result
  (s-trim-left "

Jobs (2)
  Name                                          Successful    Age
  example-job-1                                        1      10d
    Namespace:  mm
    RestartPolicy: OnFailure

    Created:    2017-04-22T22:00:02Z
    Started:    2017-04-22T22:00:03Z
    Completed:  2017-04-23T00:00:05Z

  example-job-2                                        1      10d
    Namespace:  mm
    RestartPolicy: OnFailure

    Created:    2017-04-22T22:00:02Z
    Started:    2017-04-22T22:00:03Z
    Completed:  2017-04-23T00:00:05Z


"))

(ert-deftest kubernetes-jobs-test--sample-response ()
  (let ((state `((jobs . ,sample-get-jobs-response)
                 (current-time . ,(date-to-time "2017-05-03 00:00Z")))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-jobs-section state)))
      (should (equal kubernetes-jobs-test--sample-result
                     (substring-no-properties (buffer-string)))))))

(ert-deftest kubernetes-jobs-test--sample-response-text-properties ()
  (let ((state `((jobs . ,sample-get-jobs-response))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-jobs-section state)))
      ;; Skip past header.
      (forward-line 2)

      (dolist (key '("Namespace"
                     "Created"))
        (save-excursion
          (search-forward key)
          (should (equal 'magit-header-line (get-text-property (point) 'face))))))))


;;; kubernetes-jobs-test.el ends here
