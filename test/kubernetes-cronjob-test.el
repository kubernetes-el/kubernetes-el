;;; kubernetes-cronjobs-test.el --- Test rendering of the cronjobs list  -*- lexical-binding: t; -*-

(require 'ert)
(require 'kubernetes-cronjobs)
(require 's)

(declare-function test-helper-json-resource "test-helper.el")

(defconst sample-get-cronjobs-response (test-helper-json-resource "get-cronjobs-response.json"))

(defun draw-cronjobs-section (state)
  (kubernetes-ast-eval `(cronjobs-list ,state)))

;; Shows "Fetching..." when state isn't initialized yet.

(defconst kubernetes-cronjobs-test--loading-result
  (s-trim-left "

CronJobs
  Name                                 Namespace       Schedule        Suspend
  Fetching...

"))

(ert-deftest kubernetes-cronjobs-test--empty-state ()
  (with-temp-buffer
    (save-excursion (magit-insert-section (root)
                      (draw-cronjobs-section nil)))
    (should (equal kubernetes-cronjobs-test--loading-result
                   (substring-no-properties (buffer-string))))
    (forward-line 1)
    (forward-to-indentation)
    (should (equal 'kubernetes-progress-indicator (get-text-property (point) 'face)))))

;; Shows "None" when there are no cronjobs.

(defconst kubernetes-cronjobs-test--empty-result
  (s-trim-left "

CronJobs (0)
  None.

"))

(ert-deftest kubernetes-cronjobs-test--no-cronjobs ()
  (let ((empty-state `((cronjobs . ((items . ,(vector)))))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-cronjobs-section empty-state)))
      (should (equal kubernetes-cronjobs-test--empty-result
                     (substring-no-properties (buffer-string))))
      (search-forward "None")
      (should (equal 'kubernetes-dimmed (get-text-property (point) 'face))))))

;; Shows cronjob lines when there are cronjobs.

(defconst kubernetes-cronjobs-test--sample-result
  (s-trim-left "

CronJobs (2)
  Name                                 Namespace       Schedule        Suspend
  cronjobs-1                           default         */5 * * * *       false
    Namespace:  default
    Name:       cronjobs-1
    Created:    2024-02-20T14:39:22Z
    Schedule:   */5 * * * *
    Suspend:    false

  cronjobs-2                           core            @daily             true
    Namespace:  core
    Name:       cronjobs-2
    Created:    2024-11-25T07:28:31Z
    Schedule:   @daily
    Suspend:    true


"))

(ert-deftest kubernetes-cronjobs-test--sample-response ()
  (let ((state `((cronjobs . ,sample-get-cronjobs-response))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-cronjobs-section state)))
      (should (equal kubernetes-cronjobs-test--sample-result
                     (substring-no-properties (buffer-string)))))))

(ert-deftest kubernetes-cronjobs-delete-marked-test ()
  "Test deleting marked cronjobs."
  (let* ((test-state (make-hash-table :test 'equal))
         (deleted-cronjobs '())
         (state-updated nil))

    ;; Mock functions
    (cl-letf (((symbol-function 'kubernetes-state--get)
               (lambda (_state key)
                 (when (eq key 'marked-cronjobs)
                   '("cronjobs-1" "cronjobs-2"))))
              ((symbol-function 'kubernetes-state-delete-cronjob)
               (lambda (name) (push name deleted-cronjobs)))
              ((symbol-function 'kubernetes-kubectl-delete)
               (lambda (_type name _state success-fn _error-fn)
                 (funcall success-fn nil)))
              ((symbol-function 'kubernetes-state-trigger-redraw)
               (lambda () (setq state-updated t))))

      ;; Execute function
      (kubernetes-cronjobs-delete-marked test-state)

      ;; Verify results
      (should (equal (sort deleted-cronjobs #'string<)
                     (sort '("cronjobs-1" "cronjobs-2") #'string<)))
      (should state-updated))))

(ert-deftest kubernetes-cronjobs-delete-marked-failure-test ()
  "Test handling of failed cronjob deletion."
  (let* ((test-state (make-hash-table :test 'equal))
         (remarked-cronjobs '()))

    ;; Mock functions
    (cl-letf (((symbol-function 'kubernetes-state--get)
               (lambda (_state key)
                 (when (eq key 'marked-cronjobs)
                   '("cronjobs-1"))))
              ((symbol-function 'kubernetes-state-delete-cronjob)
               (lambda (_name) t))
              ((symbol-function 'kubernetes-kubectl-delete)
               (lambda (_type _name _state _success-fn error-fn)
                 (funcall error-fn nil)))
              ((symbol-function 'kubernetes-state-mark-cronjob)
               (lambda (name) (push name remarked-cronjobs))))

      ;; Execute function
      (kubernetes-cronjobs-delete-marked test-state)

      ;; Verify cronjob was remarked after failure
      (should (equal remarked-cronjobs '("cronjobs-1"))))))

;; Tests for kubernetes-cronjobs--read-name

(ert-deftest kubernetes-cronjobs--read-name-test ()
  "Test reading cronjob name with existing state."
  (let* ((test-state (make-hash-table :test 'equal))
         (completing-read-called nil)
         (completing-read-choices nil))

    ;; Mock functions
    (cl-letf (((symbol-function 'kubernetes-state--get)
               (lambda (_state _key) sample-get-cronjobs-response))
              ((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _)
                 (setq completing-read-called t
                       completing-read-choices choices)
                 "cronjobs-1")))

      ;; Execute function
      (let ((result (kubernetes-cronjobs--read-name test-state)))

      ;; Verify results
      (should completing-read-called)
      (should (equal result "cronjobs-1"))
      ;; Verify choices include both cronjobs from fixture
      (should (equal (sort completing-read-choices #'string<)
                     '("cronjobs-1" "cronjobs-2")))))))

(ert-deftest kubernetes-cronjobs--read-name-fetch-test ()
  "Test reading cronjob name when state needs to be fetched."
  (let* ((test-state (make-hash-table :test 'equal))
         (state-updated nil))

    ;; Mock functions
    (cl-letf (((symbol-function 'kubernetes-state--get)
               (lambda (_state _key) nil))
              ((symbol-function 'kubernetes-kubectl-await-on-async)
               (lambda (_state _fn) sample-get-cronjobs-response))
              ((symbol-function 'kubernetes-state-update-cronjobs)
               (lambda (_) (setq state-updated t)))
              ((symbol-function 'completing-read)
               (lambda (_prompt _choices &rest _) "cronjobs-1")))

      ;; Execute function
      (let ((result (kubernetes-cronjobs--read-name test-state)))

      ;; Verify state was updated and name was returned
      (should state-updated)
      (should (equal result "cronjobs-1"))))))

;; Tests for kubernetes-display-cronjob

(ert-deftest kubernetes-display-cronjob-test ()
  "Test displaying cronjob information."
  (let* ((test-state (make-hash-table :test 'equal))
         (displayed-cronjob nil)
         (buffer-displayed nil))

    ;; Mock functions
    (cl-letf (((symbol-function 'kubernetes-state-lookup-cronjob)
               (lambda (name _state)
                 (let* ((items (alist-get 'items sample-get-cronjobs-response))
                        (cronjob (seq-find (lambda (j)
                                       (equal name (alist-get 'name (alist-get 'metadata j))))
                                     items)))
                   (setq displayed-cronjob cronjob)
                   cronjob)))
              ((symbol-function 'kubernetes-yaml-make-buffer)
               (lambda (_name _cronjob) (generate-new-buffer "test-buffer")))
              ((symbol-function 'display-buffer)
               (lambda (_buffer) (selected-window)))
              ((symbol-function 'select-window)
               (lambda (_) (setq buffer-displayed t))))

      ;; Execute function
      (kubernetes-display-cronjob "cronjobs-1" test-state)

      ;; Verify buffer was displayed with correct cronjob
      (should buffer-displayed)
      (should displayed-cronjob)
      (should (equal (alist-get 'name (alist-get 'metadata displayed-cronjob))
                     "cronjobs-1")))))

(ert-deftest kubernetes-display-cronjob-error-test ()
  "Test error handling when displaying unknown cronjob."
  (let ((test-state (make-hash-table :test 'equal)))

    ;; Mock functions
    (cl-letf (((symbol-function 'kubernetes-state-lookup-cronjob)
               (lambda (_name _state) nil)))

      ;; Should signal error for unknown cronjob
      (should-error (kubernetes-display-cronjob "unknown-cronjob" test-state)
                    :type 'error))))

;;; kubernetes-cronjobs-test.el ends here
