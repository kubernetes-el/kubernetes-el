;;; kubernetes-state-test.el --- Tests for kubernetes-state.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (unless (bound-and-true-p test-helper-loaded)
    (load (concat default-directory "test-helper.el"))))

(require 'kubernetes-state)


(ert-deftest kubernetes-state-test--client-process-accessors ()
  (let ((process 'p)
        (kubernetes-state (kubernetes-state-empty))
        (result))
    (kubernetes-state-set-client-process process)
    (setq result (kubernetes-state-client-process))
    (should (equal process result))))

(ert-deftest kubernetes-state-test--namespace-accessors ()
  (let ((namespace "ns")
        (kubernetes-state (kubernetes-state-empty))
        (result))
    (kubernetes-state-set-namespace namespace)
    (setq result (kubernetes-state-namespace))

    (should (equal namespace result))
    (should-error (kubernetes-state-set-namespace 'symbol))))

(provide 'kubernetes-state-test)

;;; kubernetes-state-test.el ends here
