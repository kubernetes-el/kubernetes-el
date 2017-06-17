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
         (kubernetes-state-client-message-processed-functions nil))
     ,@body))

;; Clearing state

(ert-deftest kubernetes-state-test--clearing-state ()
  (kubernetes-state--with-empty-state
    (puthash 'namespace 'bar (kubernetes-state))
    (let ((pods-table (kubernetes-state-pods)))
      (puthash 'foo 'bar pods-table))
    (kubernetes-state-clear)
    (should (hash-table-empty-p (kubernetes-state-pods)))
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

(ert-deftest kubernetes-state-test--handle-client-line--handles-empty-pod-upserts ()
  (kubernetes-state--with-empty-state
    (let ((message '((type . "pod")
                     (operation . "upsert")
                     (data))))
      (kubernetes-state-handle-client-line (prin1-to-string message))
      (should (hash-table-empty-p (kubernetes-state-pods))))))

(ert-deftest kubernetes-state-test--handle-client-line--handles-nonempty-pod-upserts ()
  (kubernetes-state--with-empty-state
    (let ((message '((type . "pod")
                     (operation . "upsert")
                     (data . [((metadata (name . "A")))
                              ((metadata (name . "B")))
                              ((metadata (name . "C")))]))))

      (kubernetes-state-handle-client-line (prin1-to-string message))
      (let ((pods (kubernetes-state-pods)))
        (should pods)
        (should (hash-table-p pods))
        (should (equal 3 (hash-table-count pods)))))))

(ert-deftest kubernetes-state-test--handle-client-line--handles-empty-pod-deletes ()
  (kubernetes-state--with-empty-state
    (let ((message '((type . "pod")
                     (operation . "delete")
                     (data))))
      (kubernetes-state-handle-client-line (prin1-to-string message))
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
    (let ((message '((type . "pod")
                     (operation . "delete")
                     (data . [((metadata (name . "B")))
                              ((metadata (name . "C")))
                              ((metadata (name . "Z")))]))))

      (kubernetes-state-handle-client-line (prin1-to-string message))
      (let ((pods (kubernetes-state-pods)))
        (should pods)
        (should (hash-table-p pods))
        (should (equal 2 (hash-table-count pods)))))))


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

(ert-deftest kubernetes-state-test--pods-accessors ()
  (kubernetes-state--with-empty-state
    (let ((pods 'pods)
          (result))
      (kubernetes-state-set-pods pods)
      (setq result (kubernetes-state-pods))

      (should (equal pods result)))))

(ert-deftest kubernetes-state-test--updates-received-p-accessors ()
  (kubernetes-state--with-empty-state
    (should-not (kubernetes-state-updates-received-p))
    (kubernetes-state-set-updates-received-p t)
    (should (kubernetes-state-updates-received-p))))

(provide 'kubernetes-state-test)

;;; kubernetes-state-test.el ends here
