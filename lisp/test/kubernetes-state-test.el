;;; kubernetes-state-test.el --- Tests for kubernetes-state.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (unless (bound-and-true-p test-helper-loaded)
    (load (concat default-directory "test-helper.el"))))

(require 'subr-x)

(require 'kubernetes-state)

(defmacro kubernetes-state--with-empty-state (&rest body)
  (declare (indent 0))
  `(let ((kubernetes-state (kubernetes-state-empty))
         (kubernetes-state-should-redraw-functions nil))
     ,@body))

;; Clearing state

(ert-deftest kubernetes-state-test--clearing-state ()
  (kubernetes-state--with-empty-state
    (puthash 'namespace 'bar (kubernetes-state))
    (let ((pods-table (kubernetes-state-pods)))
      (puthash 'foo 'bar pods-table))
    (let ((deployments-table (kubernetes-state-deployments)))
      (puthash 'foo 'bar deployments-table))
    (kubernetes-state-clear)
    (should (hash-table-empty-p (kubernetes-state-pods)))
    (should (hash-table-empty-p (kubernetes-state-deployments)))
    (should-not (gethash 'namespace (kubernetes-state)))))


;; Subprocess messages.

(ert-deftest kubernetes-state-test--handle-client-line--errors-if-malformed ()
  (kubernetes-state--with-empty-state
    (should-error (kubernetes-state-handle-client-line "("))))

(ert-deftest kubernetes-state-test--handle-client-line--errors-if-unknown-resource-type ()
  (kubernetes-state--with-empty-state
    (let ((message '((type . "doge")
                     (operation . "upsert")
                     (data))))
      (should-error (kubernetes-state-handle-client-line (prin1-to-string message))))))

(ert-deftest kubernetes-state-test--handle-client-line--errors-if-unknown-operation ()
  (kubernetes-state--with-empty-state
    (let ((message '((type . "pod")
                     (operation . "bark")
                     (data))))
      (should-error (kubernetes-state-handle-client-line (prin1-to-string message))))))

;; Pod messages.

(ert-deftest kubernetes-state-test--handle-client-line--handles-empty-pod-upserts ()
  (kubernetes-state--with-empty-state
    (let* ((hook-run-p)
           (kubernetes-state-should-redraw-functions (list (lambda (_) (setq hook-run-p t))))
           (message '((type . "pod")
                      (operation . "upsert")
                      (data))))
      (kubernetes-state-handle-client-line (prin1-to-string message))
      (should-not hook-run-p)
      (should (hash-table-empty-p (kubernetes-state-pods))))))

(ert-deftest kubernetes-state-test--handle-client-line--handles-nonempty-pod-upserts ()
  (kubernetes-state--with-empty-state
    (let* ((hook-run-p)
           (kubernetes-state-should-redraw-functions (list (lambda (_) (setq hook-run-p t))))
           (message '((type . "pod")
                      (operation . "upsert")
                      (data . [((metadata (name . "A")))
                               ((metadata (name . "B")))
                               ((metadata (name . "C")))]))))

      (kubernetes-state-handle-client-line (prin1-to-string message))
      (should hook-run-p)
      (let ((pods (kubernetes-state-pods)))
        (should pods)
        (should (hash-table-p pods))
        (should (equal 3 (hash-table-count pods)))))))

(ert-deftest kubernetes-state-test--handle-client-line--handles-empty-pod-deletes ()
  (kubernetes-state--with-empty-state
    (let* ((hook-run-p)
           (kubernetes-state-should-redraw-functions (list (lambda (_) (setq hook-run-p t))))
           (message '((type . "pod")
                      (operation . "delete")
                      (data))))

      (kubernetes-state-handle-client-line (prin1-to-string message))
      (should-not hook-run-p)
      (should (hash-table-empty-p (kubernetes-state-pods))))))

(ert-deftest kubernetes-state-test--handle-client-line--handles-nonempty-pod-deletes ()
  (kubernetes-state--with-empty-state
    ;; Populate the state with some pods.
    (let ((pods-table (kubernetes-state-pods)))
      (puthash 'A t pods-table)
      (puthash 'B t pods-table)
      (puthash 'C t pods-table)
      (puthash 'D t pods-table))

    ;; Test that individual pods are deleted from the state.
    (let* ((hook-run-p)
           (kubernetes-state-should-redraw-functions (list (lambda (_) (setq hook-run-p t))))
           (message '((type . "pod")
                      (operation . "delete")
                      (data . [((metadata (name . "B")))
                               ((metadata (name . "C")))
                               ((metadata (name . "Z")))]))))

      (kubernetes-state-handle-client-line (prin1-to-string message))
      (should hook-run-p)
      (let ((pods (kubernetes-state-pods)))
        (should pods)
        (should (hash-table-p pods))
        (should (equal 2 (hash-table-count pods)))))))

;; Deployment messages.

(ert-deftest kubernetes-state-test--handle-client-line--handles-empty-deployment-upserts ()
  (kubernetes-state--with-empty-state
    (let* ((hook-run-p)
           (kubernetes-state-should-redraw-functions (list (lambda (_) (setq hook-run-p t))))
           (message '((type . "deployment")
                      (operation . "upsert")
                      (data))))
      (kubernetes-state-handle-client-line (prin1-to-string message))
      (should-not hook-run-p)
      (should (hash-table-empty-p (kubernetes-state-deployments))))))

(ert-deftest kubernetes-state-test--handle-client-line--handles-nonempty-deployment-upserts ()
  (kubernetes-state--with-empty-state
    (let* ((hook-run-p)
           (kubernetes-state-should-redraw-functions (list (lambda (_) (setq hook-run-p t))))
           (message '((type . "deployment")
                      (operation . "upsert")
                      (data . [((metadata (name . "A")))
                               ((metadata (name . "B")))
                               ((metadata (name . "C")))]))))

      (kubernetes-state-handle-client-line (prin1-to-string message))
      (should hook-run-p)
      (let ((deployments (kubernetes-state-deployments)))
        (should deployments)
        (should (hash-table-p deployments))
        (should (equal 3 (hash-table-count deployments)))))))

(ert-deftest kubernetes-state-test--handle-client-line--handles-empty-deployment-deletes ()
  (kubernetes-state--with-empty-state
    (let* ((hook-run-p)
           (kubernetes-state-should-redraw-functions (list (lambda (_) (setq hook-run-p t))))
           (message '((type . "deployment")
                      (operation . "delete")
                      (data))))

      (kubernetes-state-handle-client-line (prin1-to-string message))
      (should-not hook-run-p)
      (should (hash-table-empty-p (kubernetes-state-deployments))))))

(ert-deftest kubernetes-state-test--handle-client-line--handles-nonempty-deployment-deletes ()
  (kubernetes-state--with-empty-state
    ;; Populate the state with some deployments.
    (let ((deployments-table (kubernetes-state-deployments)))
      (puthash 'A t deployments-table)
      (puthash 'B t deployments-table)
      (puthash 'C t deployments-table)
      (puthash 'D t deployments-table))

    ;; Test that individual deployments are deleted from the state.
    (let* ((hook-run-p)
           (kubernetes-state-should-redraw-functions (list (lambda (_) (setq hook-run-p t))))
           (message '((type . "deployment")
                      (operation . "delete")
                      (data . [((metadata (name . "B")))
                               ((metadata (name . "C")))
                               ((metadata (name . "Z")))]))))

      (kubernetes-state-handle-client-line (prin1-to-string message))
      (should hook-run-p)
      (let ((deployments (kubernetes-state-deployments)))
        (should deployments)
        (should (hash-table-p deployments))
        (should (equal 2 (hash-table-count deployments)))))))


;; Error messages.

(ert-deftest kubernetes-state-test--handle-client-line--handles-error-messages ()
  (kubernetes-state--with-empty-state
    (let* ((hook-run-p)
           (kubernetes-state-should-redraw-functions (list (lambda (_) (setq hook-run-p t))))
           (message
            '((type . "error")
              (msg . "Could not get pods")
              (error . "Get https://example.com:8443/api/v1/namespaces/mm/pods: read tcp 10.0.0.0:0->10.0.0.0:0: read: operation timed out"))))

      (kubernetes-state-handle-client-line (prin1-to-string message))
      (should hook-run-p)
      (should (equal message (kubernetes-state-error))))))


;; Accessors.

(ert-deftest kubernetes-state-test--defaccessors--validates-args ()
  (should-error (eval '(kubernetes-state-defaccessors)))
  (should-error (eval '(kubernetes-state-defaccessors "invalid" ())))
  (should-error (eval '(kubernetes-state-defaccessors --test "invalid")))
  (should-error (eval '(kubernetes-state-defaccessors --test ()))))

(ert-deftest kubernetes-state-test--client-process-accessors ()
  (kubernetes-state--with-empty-state
    (let ((process 'p)
          (result))
      (kubernetes-state-set-client-process process)
      (setq result (kubernetes-state-client-process))
      (should (equal process result)))))

(ert-deftest kubernetes-state-test--namespace-accessors ()
  (kubernetes-state--with-empty-state
    (let ((namespace "ns")
          (result))
      (kubernetes-state-set-namespace namespace)
      (setq result (kubernetes-state-namespace))

      (should (equal namespace result))
      (should-error (kubernetes-state-set-namespace 'symbol)))))

(ert-deftest kubernetes-state-test--overview-mode-accessors ()
  (kubernetes-state--with-empty-state
    (let ((overview-mode 'pods-list)
          (result))
      (kubernetes-state-set-overview-mode overview-mode)
      (setq result (kubernetes-state-overview-mode))

      (should (equal overview-mode result))
      (should-error (kubernetes-state-set-overview-mode 'unknown)))))

(ert-deftest kubernetes-state-test--context-accessors ()
  (kubernetes-state--with-empty-state
    (let ((context "ns")
          (result))
      (kubernetes-state-set-context context)
      (setq result (kubernetes-state-context))

      (should (equal context result))
      (should-error (kubernetes-state-set-context 'symbol)))))

(ert-deftest kubernetes-state-test--cluster-accessors ()
  (kubernetes-state--with-empty-state
    (let ((cluster "ns")
          (result))
      (kubernetes-state-set-cluster cluster)
      (setq result (kubernetes-state-cluster))

      (should (equal cluster result))
      (should-error (kubernetes-state-set-cluster 'symbol)))))

(ert-deftest kubernetes-state-test--user-accessors ()
  (kubernetes-state--with-empty-state
    (let ((user "ns")
          (result))
      (kubernetes-state-set-user user)
      (setq result (kubernetes-state-user))

      (should (equal user result))
      (should-error (kubernetes-state-set-user 'symbol)))))

(ert-deftest kubernetes-state-test--pods-accessors ()
  (kubernetes-state--with-empty-state
    (let ((pods 'pods)
          (result))
      (kubernetes-state-set-pods pods)
      (setq result (kubernetes-state-pods))

      (should (equal pods result)))))

(ert-deftest kubernetes-state-test--deployments-accessors ()
  (kubernetes-state--with-empty-state
    (let ((deployments 'deployments)
          (result))
      (kubernetes-state-set-deployments deployments)
      (setq result (kubernetes-state-deployments))

      (should (equal deployments result)))))

(ert-deftest kubernetes-state-test--data-received-p-accessors ()
  (kubernetes-state--with-empty-state
    (should-not (kubernetes-state-data-received-p))
    (kubernetes-state-set-data-received-p t)
    (should (kubernetes-state-data-received-p))))

(ert-deftest kubernetes-state-test--error-accessors ()
  (kubernetes-state--with-empty-state
    (let ((err "nope")
          (result))
      (kubernetes-state-set-error err)
      (setq result (kubernetes-state-error))

      (should (equal err result)))))


;; Resetting state to kubeconfig defaults

(ert-deftest kubernetes-state-test--resetting ()
  (kubernetes-state--with-empty-state
    (let ((props `((kubeconfig-settings
                    . (lambda ()
                        '((user . "user")
                          (context . "context")
                          (cluster . "cluster")
                          (namespace . "ns")))))))
      (kubernetes-state-set-namespace "before")
      (kubernetes-state-reset props)
      (should (equal (kubernetes-state-namespace) "ns"))
      (should (equal (kubernetes-state-context) "context"))
      (should (equal (kubernetes-state-cluster) "cluster"))
      (should (equal (kubernetes-state-user) "user")))))

(ert-deftest kubernetes-state-test--resetting-resources-only ()
  (kubernetes-state--with-empty-state
    (kubernetes-state-set-namespace "ns")
    (kubernetes-state-set-data-received-p t)
    (kubernetes-state-set-overview-mode 'pods-list)

    ;; Populate the state with some resources.
    (let ((pods-table (kubernetes-state-pods)))
      (puthash 'A t pods-table)
      (puthash 'B t pods-table)
      (puthash 'C t pods-table)
      (puthash 'D t pods-table))
    (let ((deployments-table (kubernetes-state-deployments)))
      (puthash 'A t deployments-table)
      (puthash 'B t deployments-table)
      (puthash 'C t deployments-table)
      (puthash 'D t deployments-table))

    (kubernetes-state-reset-resources)
    (should (equal (kubernetes-state-namespace) "ns"))
    (should (equal (kubernetes-state-overview-mode) 'pods-list))
    (should-not (kubernetes-state-data-received-p))
    (should (hash-table-empty-p (kubernetes-state-pods)))
    (should (hash-table-empty-p (kubernetes-state-deployments)))))


(provide 'kubernetes-state-test)

;;; kubernetes-state-test.el ends here
