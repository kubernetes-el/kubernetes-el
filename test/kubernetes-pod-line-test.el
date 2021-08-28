;;; kubernetes-pod-line-test.el --- Test rendering of the pods lines  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'kubernetes-pod-line)

(declare-function test-helper-json-resource "test-helper.el")

(defconst sample-get-pods-response (test-helper-json-resource "get-pods-response.json"))

(ert-deftest kubernetes-pod-line-test--pod-line-ok-p ()
  (let* ((state `((pods . ,sample-get-pods-response)))
         (pods (kubernetes-state--get state 'pods))
         (pod (aref (alist-get 'items pods) 0)))
    (should-not (null (kubernetes-pod-line-ok-p pod)))))

;;; kubernetes-pod-line-test.el ends here
