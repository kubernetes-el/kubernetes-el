;;; kubernetes-config-test.el --- Tests for kubernetes-config-popup.el  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (unless (bound-and-true-p test-helper-loaded)
    (load (concat default-directory "test-helper.el"))))

(require 'kubernetes-config)

;; Setting namespace

(ert-deftest kubernetes-config-test--set-namespace--restarts-client-if-changed ()
  (let* ((current-ns)
         (restarted)
         (resources-reset)
         (new-ns (make-symbol "new-ns"))
         (props `((get-namespace . ,(lambda () current-ns))
                  (set-namespace . ,(lambda (value) (setq current-ns value)))
                  (restart-client . ,(lambda () (setq restarted t)))
                  (reset-resources . ,(lambda () (setq resources-reset t))))))
    (kubernetes-config-set-namespace new-ns props)
    (should (equal new-ns current-ns))
    (should resources-reset)
    (should restarted)))


(ert-deftest kubernetes-config-test--set-namespace--no-restart-if-unchanged ()
  (let* ((restarted)
         (resources-reset)
         (current-ns "foo")
         (props `((get-namespace . ,(lambda () current-ns))
                  (restart-client . ,(lambda () (setq restarted t)))
                  (reset-resources . ,(lambda () (setq resources-reset t)))
                  (set-namespace))))
    (kubernetes-config-set-namespace current-ns props)
    (should-not restarted)
    (should-not resources-reset)))


;; Setting context

(ert-deftest kubernetes-config-test--set-context--restarts-client-if-changed ()
  (let* ((restarted-p)
         (state-cleared-p)
         (state-set-p)
         (state-updated-p)
         (new-context (make-symbol "new-context"))
         (props `((get-context
                   . ,(lambda () "current-context"))
                  (restart-client
                   . ,(lambda () (setq restarted-p t)))
                  (lookup-kubectl-settings
                   . (lambda (&rest _) t))
                  (populate-from-kubectl-settings
                   . ,(lambda (&rest _)
                        (setq state-updated-p t)
                        (setq state-set-p t)))
                  (clear-state
                   . ,(lambda ()
                        (setq state-cleared-p t)
                        (setq state-set-p nil))))))
    (kubernetes-config-set-context new-context props)
    (should state-cleared-p)
    (should state-updated-p)
    (should state-set-p)
    (should restarted-p)))


(ert-deftest kubernetes-config-test--set-context--no-restart-if-unchanged ()
  (let* ((restarted-p)
         (state-cleared-p)
         (current-context "foo")
         (props `((get-context
                   . ,(lambda () current-context))
                  (restart-client
                   . ,(lambda () (setq restarted-p t)))
                  (lookup-kubectl-settings
                   . (lambda (&rest _) (error "Unexpected: lookup-kubectl-settings")))
                  (populate-from-kubectl-settings
                   . (lambda (&rest _) (error "Unexpected: populate-from-kubectl-settings")))
                  (clear-state
                   . ,(lambda () (setq state-cleared-p t))))))
    (kubernetes-config-set-context current-context props)
    (should-not restarted-p)
    (should-not state-cleared-p)))


(provide 'kubernetes-config-test)

;;; kubernetes-config-test.el ends here
