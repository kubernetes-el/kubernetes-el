;;; kubernetes-state-test.el --- Tests for application state management.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'kubernetes-state)
(declare-function test-helper-json-resource "test-helper.el")

;; Sample responses.

(defconst sample-get-pods-response
  (test-helper-json-resource "get-pods-response.json"))

(defconst sample-get-configmaps-response
  (test-helper-json-resource "get-configmaps-response.json"))

(defconst sample-get-secrets-response
  (test-helper-json-resource "get-secrets-response.json"))

(defconst sample-get-services-response
  (test-helper-json-resource "get-services-response.json"))

(defconst sample-get-namespaces-response
  (test-helper-json-resource "get-namespaces-response.json"))

(defconst sample-get-deployments-response
  (test-helper-json-resource "get-deployments-response.json"))

(defconst sample-get-jobs-response
  (test-helper-json-resource "get-jobs-response.json"))

(defconst sample-get-config-response
  (test-helper-json-resource "config-view-response.json"))

;; Updating sets the main state

(ert-deftest kubernetes-state-test--updating-and-retrieving-state ()
  (test-helper-with-empty-state
    (let ((updated (kubernetes-state-update :update-last-error 'test)))
      (should kubernetes-state--current-state)
      (should (equal updated kubernetes-state--current-state))
      (should (equal updated (kubernetes-state))))))


;; Test basic sanitisation of state transformations.

(ert-deftest kubernetes-state-test--rejects-unknown-actions ()
  (should-error (kubernetes-state-next nil :foo)))


;; Test clearing state.

(ert-deftest kubernetes-state-test--clearing-state ()
  (let ((kubernetes-state--current-state 'test))
    (kubernetes-state-clear)
    (should (not (kubernetes-state)))))


;; Test getters

(defmacro kubernetes-state-test-getter (attr)
  (let ((test-name (intern (format "kubernetes-state-test-getter-for-%s" attr)))
        (get-fname (intern (format "kubernetes-state-%s" attr))))
    `(ert-deftest ,test-name ()
       (test-helper-with-empty-state
         (let* ((value '("foo" "bar" "baz"))
                (state (list (cons (quote ,attr) value))))
           (should (equal value (,get-fname state))))))))

(kubernetes-state-test-getter marked-configmaps)
(kubernetes-state-test-getter configmaps-pending-deletion)

(kubernetes-state-test-getter marked-pods)
(kubernetes-state-test-getter pods-pending-deletion)

(kubernetes-state-test-getter marked-secrets)
(kubernetes-state-test-getter secrets-pending-deletion)

(kubernetes-state-test-getter marked-services)
(kubernetes-state-test-getter services-pending-deletion)

(kubernetes-state-test-getter marked-deployments)
(kubernetes-state-test-getter deployments-pending-deletion)

(kubernetes-state-test-getter current-time)


;; Test simple state transitions.

(defmacro kubernetes-state-test-accessor (attr update)
  (declare (indent 1))
  (let ((test-name (intern (format "kubernetes-state-test-accessors-for-%s" attr)))
        (get-fname (intern (format "kubernetes-state-%s" attr))))

    `(ert-deftest ,test-name ()
       (test-helper-with-empty-state
         (let ((update-result ,update)
               (get-result (,get-fname (kubernetes-state))))
           (should (kubernetes-state))
           (should update-result)
           (should get-result)
           (should (equal update-result get-result)))))))

(kubernetes-state-test-accessor current-namespace
  (kubernetes-state-update-current-namespace "example-ns"))

(kubernetes-state-test-accessor last-error
  (kubernetes-state-update-last-error "Foo" "Cmd" (current-time)))

(kubernetes-state-test-accessor pods
  (kubernetes-state-update-pods sample-get-pods-response))

(kubernetes-state-test-accessor configmaps
  (kubernetes-state-update-configmaps sample-get-configmaps-response))

(kubernetes-state-test-accessor secrets
  (kubernetes-state-update-secrets sample-get-secrets-response))

(kubernetes-state-test-accessor deployments
  (kubernetes-state-update-deployments sample-get-deployments-response))

(kubernetes-state-test-accessor services
  (kubernetes-state-update-services sample-get-services-response))

(kubernetes-state-test-accessor namespaces
  (kubernetes-state-update-namespaces sample-get-namespaces-response))

(kubernetes-state-test-accessor config
  (kubernetes-state-update-config sample-get-config-response))

(kubernetes-state-test-accessor kubectl-flags
  (kubernetes-state-update-kubectl-flags '("--key=value")))

(kubernetes-state-test-accessor overview-sections
  (kubernetes-state-update-overview-sections '(configmaps)))

(kubernetes-state-test-accessor jobs
  (kubernetes-state-update-jobs sample-get-jobs-response))

(ert-deftest kubernetes-state-test--error-is-alist ()
  (test-helper-with-empty-state
    (let ((time (current-time)))
      (kubernetes-state-update-last-error "Foo" "Cmd" time)
      (should (equal
               `((message . "Foo")
                 (command . "Cmd")
                 (time . , time))
               (kubernetes-state-last-error (kubernetes-state)))))))

(kubernetes-state-test-accessor label-query
  (kubernetes-state-update-label-query "test"))


;; Test special behaviours

(ert-deftest kubernetes-state-test--update-config-also-updates-current-namespace-if-empty ()
  (let ((kubernetes-state--current-state nil))
    (kubernetes-state-update-config sample-get-config-response)
    (should (kubernetes-state-config (kubernetes-state)))
    (should (kubernetes-state-current-namespace (kubernetes-state)))))

(ert-deftest kubernetes-state-test--update-config-ignores-current-namespace-if-set ()
  (let ((kubernetes-state--current-state nil))
    (kubernetes-state-update-current-namespace "foo")
    (kubernetes-state-update-config sample-get-config-response)
    (should (kubernetes-state-config (kubernetes-state)))
    (should (equal "foo" (kubernetes-state-current-namespace (kubernetes-state))))))

(ert-deftest kubernetes-state-test--kubectl-flags-initialized-using-config-popup-vars ()
  (let ((kubernetes-kubectl-flags '("--foo=bar" "-x")))
    (should (equal kubernetes-kubectl-flags (kubernetes-state-kubectl-flags nil)))))

(ert-deftest kubernetes-state-test--kubectl-flags-not-reinitialized-if-present ()
  (let ((flags '("--foo=bar" "-x")))
    (test-helper-with-empty-state
      (kubernetes-state-update-kubectl-flags flags)
      (should (equal flags (kubernetes-state-kubectl-flags (kubernetes-state)))))))

(ert-deftest kubernetes-state-test--updating-kubectl-flags-updates-global-variable ()
  (let ((flags '("--foo=bar" "-x")))
    (test-helper-with-empty-state
      (kubernetes-state-update-kubectl-flags flags)
      (should (equal kubernetes-kubectl-flags flags)))))

(ert-deftest kubernetes-state-test--overview-sections-initialized-using-default-view-variable ()
  (let ((kubernetes-default-overview-view 'configmaps))
    (test-helper-with-empty-state
      (should (equal '(context configmaps) (kubernetes-state-overview-sections nil))))))

(ert-deftest kubernetes-state-test--overview-sections-not-reinitialized-if-present ()
  (let ((sections '(context)))
    (test-helper-with-empty-state
      (kubernetes-state-update-overview-sections sections)
      (should (equal sections (kubernetes-state-overview-sections (kubernetes-state)))))))


;; Test marking/unmarking/deleting actions

(defmacro kubernetes-state-marking-tests (resource)
  (let ((get-marks-fn (intern (format "kubernetes-state-marked-%ss" resource)))
        (get-pending-deletion-fn (intern (format "kubernetes-state-%ss-pending-deletion" resource)))
        (update-fn (intern (format "kubernetes-state-update-%ss" resource)))
        (mark-fn (intern (format "kubernetes-state-mark-%s" resource)))
        (unmark-fn (intern (format "kubernetes-state-unmark-%s" resource)))
        (delete-fn (intern (format "kubernetes-state-delete-%s" resource)))
        (marks-state-key (intern (format "marked-%ss" resource))))
    `(progn

       (ert-deftest ,(intern (format "kubernetes-state-test--mark-%s" resource)) ()
         (test-helper-with-empty-state
           (,mark-fn "foo")
           (,mark-fn "bar")
           (,mark-fn "bar")
           (should (-same-items? '("foo" "bar") (,get-marks-fn (kubernetes-state))))
           (let ((sans-marked (delq (assoc (quote ,marks-state-key) kubernetes-state--current-state) kubernetes-state--current-state)))
             (should (null sans-marked)))))

       (ert-deftest ,(intern (format "kubernetes-state-test--unmark-%s" resource)) ()
         (test-helper-with-empty-state
           (,mark-fn "foo")
           (,mark-fn "bar")
           (,unmark-fn "bar")
           (should (-same-items? '("foo") (,get-marks-fn (kubernetes-state))))))

       (ert-deftest ,(intern (format "kubernetes-state-test--delete-%s" resource)) ()
         (test-helper-with-empty-state
           (,mark-fn "foo")
           (,mark-fn "bar")
           (,delete-fn "foo")
           (,delete-fn "baz")
           (should (equal '("bar") (,get-marks-fn (kubernetes-state))))
           (should (-same-items? '("foo" "baz") (,get-pending-deletion-fn (kubernetes-state))))))

       (ert-deftest ,(intern (format "kubernetes-state-test--updating-%ss-cleans-stale-marks" resource)) ()
         (test-helper-with-empty-state
           (,mark-fn "foo")
           (,mark-fn "bar")
           (,mark-fn "baz")
           (,update-fn `((items . [((metadata . ((name . "bar"))))
                                   ((metadata . ((name . "baz"))))])))
           (should (-same-items? '("bar" "baz") (,get-marks-fn (kubernetes-state))))))

       (ert-deftest ,(intern (format "kubernetes-state-test--updating-%ss-cleans-stale-deletions" resource)) ()
         (test-helper-with-empty-state
           (,delete-fn "foo")
           (,delete-fn "bar")
           (,delete-fn "baz")
           (,update-fn `((items . [((metadata . ((name . "bar"))))
                                   ((metadata . ((name . "baz"))))])))
           (should (-same-items? '("bar" "baz") (,get-pending-deletion-fn (kubernetes-state)))))))))

(kubernetes-state-marking-tests configmap)
(kubernetes-state-marking-tests deployment)
(kubernetes-state-marking-tests job)
(kubernetes-state-marking-tests pod)
(kubernetes-state-marking-tests secret)
(kubernetes-state-marking-tests service)

(ert-deftest kubernetes-state-test-unmark-all ()
  (test-helper-with-empty-state
    (kubernetes-state-mark-configmap "configmap")
    (kubernetes-state-mark-deployment "deployment")
    (kubernetes-state-mark-job "job")
    (kubernetes-state-mark-pod "pod")
    (kubernetes-state-mark-secret "secret")
    (kubernetes-state-mark-service "svc")
    (kubernetes-state-unmark-all)
    (should-not (kubernetes-state))))


;; Convenience functions

(ert-deftest kubernetes-state-test--clear-error-if-stale--not-stale ()
  (test-helper-with-empty-state
    (let* ((time (current-time))
           (kubernetes-state--current-state `((last-error . ((time . ,time))))))
      (kubernetes-state-clear-error-if-stale 5)
      (should (kubernetes-state-last-error (kubernetes-state))))))

(ert-deftest kubernetes-state-test--clear-error-if-stale--stale ()
  (test-helper-with-empty-state
    (let* ((time (time-subtract (current-time) (time-to-seconds 10)))
           (kubernetes-state--current-state `((last-error . ((time . ,time))))))
      (kubernetes-state-clear-error-if-stale 5)
      (should (null (kubernetes-state-last-error (kubernetes-state)))))))

(ert-deftest kubernetes-state-test--lookup-pod--no-such-pod ()
  (test-helper-with-empty-state
    (should (not (kubernetes-state-lookup-pod "example" (kubernetes-state))))))

(ert-deftest kubernetes-state-test--lookup-pod--existing-pod ()
  (test-helper-with-empty-state
    (kubernetes-state-update-pods sample-get-pods-response)
    (should (kubernetes-state-lookup-pod "example-svc-v3-1603416598-2f9lb" (kubernetes-state)))))

(ert-deftest kubernetes-state-test--lookup-job--no-such-job ()
  (test-helper-with-empty-state
    (should (not (kubernetes-state-lookup-job "example" (kubernetes-state))))))

(ert-deftest kubernetes-state-test--lookup-job--existing-job ()
  (test-helper-with-empty-state
    (kubernetes-state-update-jobs sample-get-jobs-response)
    (should (kubernetes-state-lookup-job "example-job-1" (kubernetes-state)))))

(ert-deftest kubernetes-state-test--lookup-configmap--no-such-configmap ()
  (test-helper-with-empty-state
    (should (not (kubernetes-state-lookup-configmap "example" (kubernetes-state))))))

(ert-deftest kubernetes-state-test--lookup-configmap--existing-configmap ()
  (test-helper-with-empty-state
    (kubernetes-state-update-configmaps sample-get-configmaps-response)
    (should (kubernetes-state-lookup-configmap "example-configmap-1" (kubernetes-state)))))

(ert-deftest kubernetes-state-test--lookup-secret--no-such-secret ()
  (test-helper-with-empty-state
    (should (not (kubernetes-state-lookup-secret "example" (kubernetes-state))))))

(ert-deftest kubernetes-state-test--lookup-secret--existing-secret ()
  (test-helper-with-empty-state
    (kubernetes-state-update-secrets sample-get-secrets-response)
    (should (kubernetes-state-lookup-secret "example-secret-1" (kubernetes-state)))))

(ert-deftest kubernetes-state-test--lookup-service--no-such-service ()
  (test-helper-with-empty-state
    (should (not (kubernetes-state-lookup-service "example" (kubernetes-state))))))

(ert-deftest kubernetes-state-test--lookup-service--existing-service ()
  (test-helper-with-empty-state
    (kubernetes-state-update-services sample-get-services-response)
    (should (kubernetes-state-lookup-service "example-svc-1" (kubernetes-state)))))

(ert-deftest kubernetes-state-test--lookup-deployment--no-such-deployment ()
  (test-helper-with-empty-state
    (should (not (kubernetes-state-lookup-deployment "example" (kubernetes-state))))))

(ert-deftest kubernetes-state-test--lookup-deployment--existing-deployment ()
  (test-helper-with-empty-state
    (kubernetes-state-update-deployments sample-get-deployments-response)
    (should (kubernetes-state-lookup-deployment "deployment-1" (kubernetes-state)))))

(ert-deftest kubernetes-state-test--current-context--no-context-set ()
  (test-helper-with-empty-state
    (should (not (kubernetes-state-current-context (kubernetes-state))))))

(ert-deftest kubernetes-state-test--current-context--current-context-set ()
  (test-helper-with-empty-state
    (kubernetes-state-update-config sample-get-config-response)
    (should (kubernetes-state-current-context (kubernetes-state)))))

(ert-deftest kubernetes-state-test--lookup-namespace--no-such-namespace ()
  (test-helper-with-empty-state
    (should (not (kubernetes-state-lookup-namespace "example" (kubernetes-state))))))

(ert-deftest kubernetes-state-test--lookup-namespace--existing-namespace ()
  (test-helper-with-empty-state
    (kubernetes-state-update-namespaces sample-get-namespaces-response)
    (should (kubernetes-state-lookup-namespace "ns2" (kubernetes-state)))))


(provide 'kubernetes-state-test)

;;; kubernetes-state-test.el ends here
