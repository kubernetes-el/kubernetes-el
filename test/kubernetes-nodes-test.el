;;; kubernetes-nodes-test.el --- Test rendering of the nodes list  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'kubernetes-nodes)
(declare-function test-helper-json-resource "test-helper.el")


(defconst sample-get-nodes-response (test-helper-json-resource "get-nodes-response.json"))

(defun draw-nodes-section (state)
  (kubernetes-ast-eval `(nodes-list ,state)))


;; Shows "Fetching..." when state isn't initialized yet.

(defconst kubernetes-nodes-test--loading-result
  (s-trim-left "

Nodes
  Name                                          Status     Roles     Age   Version
  Fetching...

"))

(ert-deftest kubernetes-nodes-test--empty-state ()
  (with-temp-buffer
    (save-excursion (magit-insert-section (root)
                      (draw-nodes-section nil)))
    (should (equal kubernetes-nodes-test--loading-result
                   (substring-no-properties (buffer-string))))
    (forward-line 1)
    (forward-to-indentation)
    (should (equal 'kubernetes-progress-indicator (get-text-property (point) 'face)))))


;; Shows "None" when there are no nodes.

(defconst kubernetes-nodes-test--empty-result
  (s-trim-left "

Nodes (0)
  None.

"))

(ert-deftest kubernetes-nodes-test--no-nodes ()
  (let ((empty-state `((nodes . ((items . ,(vector)))))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-nodes-section empty-state)))
      (should (equal kubernetes-nodes-test--empty-result
                     (substring-no-properties (buffer-string))))
      (search-forward "None")
      (should (equal 'magit-dimmed (get-text-property (point) 'face))))))

;; Shows node lines when there are nodes.

(defconst kubernetes-nodes-test--sample-result
  (s-trim-left "

Nodes (1)
  Name                                          Status     Roles     Age   Version
  minikube                                      Ready      master    -3y   v1.16.2
    Name:       minikube
    InternalIP: 192.168.39.216
    Hostname:   minikube
    architecture: amd64
    bootID:     a262aebb-5842-4b3b-9000-f0ca0bf83080
    containerRuntimeVersion: docker://18.9.9
    kernelVersion: 4.19.76
    kubeProxyVersion: v1.16.2
    kubeletVersion: v1.16.2
    machineID:  dbad2f9a447c4b509499c31fd4f076ac
    operatingSystem: linux
    osImage:    Buildroot 2019.02.6
    systemUUID: dbad2f9a-447c-4b50-9499-c31fd4f076ac


"))

(ert-deftest kubernetes-nodes-test--sample-response ()
  (let ((state `((nodes . ,sample-get-nodes-response)
                 (current-time . ,(date-to-time "2017-04-03 00:00Z")))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-nodes-section state)))
      (should (equal kubernetes-nodes-test--sample-result
                     (substring-no-properties (buffer-string)))))))

(ert-deftest kubernetes-nodes-test--sample-response-text-properties ()
  (let ((state `((nodes . ,sample-get-nodes-response)
                 (current-time . ,(date-to-time "2017-04-03 00:00Z")))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-nodes-section state)))
      ;; Skip past header.
      (forward-line 2)
      (dolist (key '("Name"
                     "InternalIP"
                     "Hostname"
                     "architecture"
                     "bootID"
                     "osImage"
                     "systemUUID"))
        (save-excursion
          (search-forward key)
          (should (equal 'magit-header-line (get-text-property (point) 'face))))))))

;;; kubernetes-nodes-test.el ends here
