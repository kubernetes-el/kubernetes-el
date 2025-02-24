;;; kubernetes-networkpolicies-test.el --- Test rendering of the network policies list  -*- lexical-binding: t; show-trailing-whitespace: t; -*-
;;; Commentary:
;;; Code:

(require 'ert)
(require 'kubernetes-networkpolicies)
(require 's)

(declare-function test-helper-json-resource "test-helper.el")

(defconst sample-get-networkpolicies-response (test-helper-json-resource "get-networkpolicies-response.json"))

(defun draw-networkpolicies-section (state)
  (kubernetes-ast-eval `(networkpolicies-list ,state)))

;; Shows "Fetching..." when state isn't initialized yet.

(defconst kubernetes-networkpolicies-test--loading-result
  (s-trim-left "Network Policies
  Name                                 Namespace       Ingress Egress
  Fetching...

"))

(ert-deftest kubernetes-networkpolicies-test--empty-state ()
  (with-temp-buffer
    (save-excursion (magit-insert-section (root)
                      (draw-networkpolicies-section nil)))
    (should (equal kubernetes-networkpolicies-test--loading-result
                   (substring-no-properties (buffer-string))))
    (forward-line 1)
    (forward-to-indentation)
    (should (equal 'kubernetes-progress-indicator (get-text-property (point) 'face)))))

;; Shows "None" when there are no network policies.

(defconst kubernetes-networkpolicies-test--empty-result
  (s-trim-left "

Network Policies (0)
  None.

"))

(ert-deftest kubernetes-networkpolicies-test--no-networkpolicies ()
  (let ((empty-state `((networkpolicies . ((items . ,(vector)))))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-networkpolicies-section empty-state)))
      (should (equal kubernetes-networkpolicies-test--empty-result
                     (substring-no-properties (buffer-string))))
      (search-forward "None")
      (should (equal 'kubernetes-dimmed (get-text-property (point) 'face))))))

;; Shows network policy lines when there are network policies.

(defconst kubernetes-networkpolicies-test--sample-result
  (s-trim-left "

Network Policies (2)
  Name                                 Namespace       Ingress Egress
  kube-prometheus-stack-grafana        monitoring          yes     no
    Namespace:  monitoring
    Name:       kube-prometheus-stack-grafana
    Created:    2025-02-13T07:40:18Z

  kube-prometheus-stack-prometheus     monitoring          yes    yes
    Namespace:  monitoring
    Name:       kube-prometheus-stack-prometheus
    Created:    2025-02-13T07:40:18Z


"))

(ert-deftest kubernetes-networkpolicies-test--sample-response ()
  (let ((state `((networkpolicies . ,sample-get-networkpolicies-response))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-networkpolicies-section state)))
      (should (equal kubernetes-networkpolicies-test--sample-result
                     (substring-no-properties (buffer-string)))))))

(ert-deftest kubernetes-networkpolicies-delete-marked-test ()
  "Test deleting marked networkpolicies."
  (let* ((test-state (make-hash-table :test 'equal))
         (deleted-networkpolicies '())
         (state-updated nil))

    ;; Mock functions
    (cl-letf (((symbol-function 'kubernetes-state--get)
               (lambda (_state key)
                 (when (eq key 'marked-networkpolicies)
                   '("kube-prometheus-stack-grafana" "kube-prometheus-stack-prometheus"))))
              ((symbol-function 'kubernetes-state-delete-networkpolicy)
               (lambda (name) (push name deleted-networkpolicies)))
              ((symbol-function 'kubernetes-kubectl-delete)
               (lambda (_type name _state success-fn _error-fn)
                 (funcall success-fn nil)))
              ((symbol-function 'kubernetes-state-trigger-redraw)
               (lambda () (setq state-updated t))))

      ;; Execute function
      (kubernetes-networkpolicies-delete-marked test-state)

      ;; Verify results
      (should (equal (sort deleted-networkpolicies #'string<)
                     (sort '("kube-prometheus-stack-grafana" "kube-prometheus-stack-prometheus") #'string<)))
      (should state-updated))))

(ert-deftest kubernetes-networkpolicies-delete-marked-failure-test ()
  "Test handling of failed networkpolicy deletion."
  (let* ((test-state (make-hash-table :test 'equal))
         (remarked-networkpolicies '()))

    ;; Mock functions
    (cl-letf (((symbol-function 'kubernetes-state--get)
               (lambda (_state key)
                 (when (eq key 'marked-networkpolicies)
                   '("kube-prometheus-stack-grafana"))))
              ((symbol-function 'kubernetes-state-delete-networkpolicy)
               (lambda (_name) t))
              ((symbol-function 'kubernetes-kubectl-delete)
               (lambda (_type _name _state _success-fn error-fn)
                 (funcall error-fn nil)))
              ((symbol-function 'kubernetes-state-mark-networkpolicy)
               (lambda (name) (push name remarked-networkpolicies))))

      ;; Execute function
      (kubernetes-networkpolicies-delete-marked test-state)

      ;; Verify networkpolicy was remarked after failure
      (should (equal remarked-networkpolicies '("kube-prometheus-stack-grafana"))))))

;; Tests for kubernetes-networkpolicies--read-name

(ert-deftest kubernetes-networkpolicies--read-name-test ()
  "Test reading networkpolicy name with existing state."
  (let* ((test-state (make-hash-table :test 'equal))
         (completing-read-called nil)
         (completing-read-choices nil))

    ;; Mock functions
    (cl-letf (((symbol-function 'kubernetes-state--get)
               (lambda (_state _key) sample-get-networkpolicies-response))
              ((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _)
                 (setq completing-read-called t
                       completing-read-choices choices)
                 "kube-prometheus-stack-grafana")))

      ;; Execute function
      (let ((result (kubernetes-networkpolicies--read-name test-state)))

      ;; Verify results
      (should completing-read-called)
      (should (equal result "kube-prometheus-stack-grafana"))
      ;; Verify choices include both networkpolicies from fixture
      (should (equal (sort completing-read-choices #'string<)
                     '("kube-prometheus-stack-grafana" "kube-prometheus-stack-prometheus")))))))

(ert-deftest kubernetes-networkpolicies--read-name-fetch-test ()
  "Test reading networkpolicy name when state needs to be fetched."
  (let* ((test-state (make-hash-table :test 'equal))
         (state-updated nil))

    ;; Mock functions
    (cl-letf (((symbol-function 'kubernetes-state--get)
               (lambda (_state _key) nil))
              ((symbol-function 'kubernetes-kubectl-await-on-async)
               (lambda (_state _fn) sample-get-networkpolicies-response))
              ((symbol-function 'kubernetes-state-update-networkpolicies)
               (lambda (_) (setq state-updated t)))
              ((symbol-function 'completing-read)
               (lambda (_prompt _choices &rest _) "kube-prometheus-stack-grafana")))

      ;; Execute function
      (let ((result (kubernetes-networkpolicies--read-name test-state)))

      ;; Verify state was updated and name was returned
      (should state-updated)
      (should (equal result "kube-prometheus-stack-grafana"))))))

;; Tests for kubernetes-display-networkpolicy

(ert-deftest kubernetes-display-networkpolicy-test ()
  "Test displaying networkpolicy information."
  (let* ((test-state (make-hash-table :test 'equal))
         (displayed-networkpolicy nil)
         (buffer-displayed nil))

    ;; Mock functions
    (cl-letf (((symbol-function 'kubernetes-state-lookup-networkpolicy)
               (lambda (name _state)
                 (let* ((items (alist-get 'items sample-get-networkpolicies-response))
                        (networkpolicy (seq-find (lambda (j)
                                       (equal name (alist-get 'name (alist-get 'metadata j))))
                                     items)))
                   (setq displayed-networkpolicy networkpolicy)
                   networkpolicy)))
              ((symbol-function 'kubernetes-yaml-make-buffer)
               (lambda (_name _networkpolicy) (generate-new-buffer "test-buffer")))
              ((symbol-function 'display-buffer)
               (lambda (_buffer) (selected-window)))
              ((symbol-function 'select-window)
               (lambda (_) (setq buffer-displayed t))))

      ;; Execute function
      (kubernetes-display-networkpolicy "kube-prometheus-stack-grafana" test-state)

      ;; Verify buffer was displayed with correct networkpolicy
      (should buffer-displayed)
      (should displayed-networkpolicy)
      (should (equal (alist-get 'name (alist-get 'metadata displayed-networkpolicy))
                     "kube-prometheus-stack-grafana")))))

(ert-deftest kubernetes-display-networkpolicy-error-test ()
  "Test error handling when displaying unknown networkpolicy."
  (let ((test-state (make-hash-table :test 'equal)))

    ;; Mock functions
    (cl-letf (((symbol-function 'kubernetes-state-lookup-networkpolicy)
               (lambda (_name _state) nil)))

      ;; Should signal error for unknown networkpolicy
      (should-error (kubernetes-display-networkpolicy "unknown-networkpolicy" test-state)
                    :type 'error))))

;;; kubernetes-networkpolicies-test.el ends here
