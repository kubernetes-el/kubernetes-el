;;; kubernetes-overview-test.el --- Tests for kubernetes-overview.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'kubernetes-overview)
(require 'kubernetes-pods)

(declare-function test-helper-json-resource "test-helper.el")

(defconst kubernetes-overview-test--sample-configmaps-response (test-helper-json-resource "get-configmaps-response.json"))
(defconst kubernetes-overview-test--sample-deployments-response (test-helper-json-resource "get-deployments-response.json"))
(defconst kubernetes-overview-test--sample-statefulsets-response (test-helper-json-resource "get-statefulsets-response.json"))
(defconst kubernetes-overview-test--sample-pods-response (test-helper-json-resource "get-pods-response.json"))
(defconst kubernetes-overview-test--sample-secrets-response (test-helper-json-resource "get-secrets-response.json"))
(defconst kubernetes-overview-test--sample-replicasets-response (test-helper-json-resource "get-replicasets-response.json"))


;; Uses state to determine which sections to draw

(ert-deftest kubernetes-overview-test--renders-selected-sections-from-state ()
  (test-helper-with-empty-state
   (kubernetes-state-update-overview-sections '(configmaps secrets))
   (kubernetes-ast-eval (kubernetes-overview-render (kubernetes-state)))

   (let ((expected (with-temp-buffer
                     (kubernetes-ast-eval `(section (root nil)
                                                    ,(kubernetes-errors-render (kubernetes-state))
                                                    (configmaps-list ,(kubernetes-state))
                                                    (secrets-list ,(kubernetes-state))))
                     (string-trim (substring-no-properties (buffer-string))))))
     (should (equal expected (string-trim (substring-no-properties (buffer-string))))))))


;; Aggregated overview component

(defconst kubernetes-overview-test--expected-overview--empty-state (string-trim "
Statefulsets
  Name                                            Replicas                          Age
  Fetching...

Deployments
  Name                                            Replicas   UpToDate  Available    Age
  Fetching...
"))

(ert-deftest kubernetes-overview-test--aggregated-overview--shows-pending-when-state-is-empty ()
  (test-helper-with-empty-state
    (kubernetes-ast-eval `(aggregated-view ,(kubernetes-state)))
    (should (equal kubernetes-overview-test--expected-overview--empty-state (string-trim (substring-no-properties (buffer-string)))))))


(defconst kubernetes-overview-test--expected-overview--only-deployments-set (string-trim "
Statefulsets
  Name                                            Replicas                          Age
  Fetching...

Deployments (2)
  Name                                            Replicas   UpToDate  Available    Age
  deployment-1                                         1/1          1          1    64d
    Namespace:  example-ns
    Strategy:   RollingUpdate
      Max Surge:  1
      Max Unavailable: 1
    Created:    2017-02-17T02:23:30Z

    Pods
      Replicas:   1
      Fetching...

  deployment-2                                         1/1          1          1    59d
    Namespace:  example-ns
    Strategy:   RollingUpdate
      Max Surge:  1
      Max Unavailable: 1
    Created:    2017-02-22T02:15:36Z

    Pods
      Replicas:   1
      Fetching...
"))

(ert-deftest kubernetes-overview-test--aggregated-overview--no-pods-set ()
  (test-helper-with-empty-state
    (kubernetes-state-update-deployments kubernetes-overview-test--sample-deployments-response)
    (let ((state (cons `(current-time . ,(date-to-time "2017-04-23 00:00Z"))
                       (kubernetes-state))))
      (kubernetes-ast-eval `(aggregated-view ,state)))

    (should (equal kubernetes-overview-test--expected-overview--only-deployments-set
                   (string-trim (substring-no-properties (buffer-string)))))))


;; Updated expected output for tests

(defconst kubernetes-overview-test--expected-overview--populated-state (string-trim "
Statefulsets
  Name                                            Replicas                          Age
  Fetching...

Deployments (2)
  Name                                            Replicas   UpToDate  Available    Age
  deployment-1                                         1/1          1          1    64d
    Namespace:  example-ns
    Strategy:   RollingUpdate
      Max Surge:  1
      Max Unavailable: 1
    Created:    2017-02-17T02:23:30Z

    Pods
      Replicas:   1
      Running     example-svc-v3-1603416598-2f9lb

    Configmaps
      example-configmap-1
      example-configmap-2

    Secrets
      example-secret-1
      example-secret-2

  deployment-2                                         1/1          1          1    59d
    Namespace:  example-ns
    Strategy:   RollingUpdate
      Max Surge:  1
      Max Unavailable: 1
    Created:    2017-02-22T02:15:36Z

    Pods
      Replicas:   1
      Running     example-svc-v4-1603416598-2f9lb"))


(ert-deftest kubernetes-overview-test--aggregated-overview--state-set ()
  (test-helper-with-empty-state
    (kubernetes-state-update-configmaps kubernetes-overview-test--sample-configmaps-response)
    (kubernetes-state-update-deployments kubernetes-overview-test--sample-deployments-response)
    (kubernetes-state-update-replicasets kubernetes-overview-test--sample-replicasets-response)
    (kubernetes-state-update-pods kubernetes-overview-test--sample-pods-response)
    (kubernetes-state-update-secrets kubernetes-overview-test--sample-secrets-response)
    (let ((state (cons `(current-time . ,(encode-time 0 0 0 23 4 2017))
                       (kubernetes-state))))
      (kubernetes-ast-eval `(aggregated-view ,state)))
    (should (equal kubernetes-overview-test--expected-overview--populated-state
                   (string-trim (substring-no-properties (buffer-string)))))))
