;;; kubernetes-config-test.el --- Tests for kubernetes-config-popup.el  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (unless (bound-and-true-p test-helper-loaded)
    (load (concat default-directory "test-helper.el"))))

(require 'kubernetes-config)

(ert-deftest kubernetes-config-test--set-namespace--restarts-client-if-changed ()
  (let* ((current-ns)
         (restarted)
         (state-reset)
         (new-ns (make-symbol "new-ns"))
         (props `((get-namespace . ,(lambda () current-ns))
                  (set-namespace . ,(lambda (value) (setq current-ns value)))
                  (restart-client . ,(lambda () (setq restarted t)))
                  (reset-state . ,(lambda () (setq state-reset t))))))
    (kubernetes-config-set-namespace new-ns props)
    (should (equal new-ns current-ns))
    (should state-reset)
    (should restarted)))


(ert-deftest kubernetes-config-test--set-namespace--no-restart-if-unchanged ()
  (let* ((restarted)
         (state-reset)
         (current-ns "foo")
         (props `((get-namespace . ,(lambda () current-ns))
                  (restart-client . ,(lambda () (setq restarted t)))
                  (reset-state . ,(lambda () (setq state-reset t)))
                  (set-namespace))))
    (kubernetes-config-set-namespace current-ns props)
    (should-not restarted)
    (should-not state-reset)))


(provide 'kubernetes-config-test)

;;; kubernetes-config-test.el ends here
