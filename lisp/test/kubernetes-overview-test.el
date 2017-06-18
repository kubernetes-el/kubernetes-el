;;; kubernetes-overview-test.el --- Tests for kubernetes-overview.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (unless (bound-and-true-p test-helper-loaded)
    (load (concat default-directory "test-helper.el"))))

(require 'kubernetes-overview)


;; Handling updates from the backend

(ert-deftest kubernetes-overview-test--handle-client-message--no-redraw-if-overview-killed ()
  (let* ((buffer-redrawn-p)
         (props
          `((buffer-live-p
             . (lambda (_) nil))
            (get-buffer
             . (lambda (_) t))
            (kubernetes-overview-redraw
             . (lambda (&rest _) (setq buffer-redrawn-p t)))
            (data-received-p
             . (lambda () (error "Unexpected: data-received-p"))))))

    (kubernetes-overview--handle-client-message nil props)
    (should-not buffer-redrawn-p)))

(ert-deftest kubernetes-overview-test--handle-client-message--redraws ()
  (let* ((buffer-redrawn-p)
         (props
          `((buffer-live-p
             . (lambda (_) t))
            (get-buffer
             . (lambda (_) t))
            (data-received-p
             . (lambda () t))
            (kubernetes-overview-redraw
             . ,(lambda (&rest _) (setq buffer-redrawn-p t))))))

    (kubernetes-overview--handle-client-message nil props)
    (should buffer-redrawn-p)))


;; Initializing clients and handling configuration changes

(ert-deftest kubernetes-overview-test--initialize-client--restarts-on-namespace-changed ()
  (let* ((client-started-p)
         (client-stopped-p)
         (state-reset-p)
         (client-running-p t)
         (namespace-state "initial-namespace")
         (props
          `((get-client-process
             . ,(lambda () client-running-p))
            (get-namespace
             . ,(lambda () namespace-state))
            (start-client
             . ,(lambda ()
                  (setq client-started-p t)
                  (setq client-running-p t)))
            (stop-client
             . ,(lambda ()
                  (setq client-stopped-p t)
                  (setq client-running-p nil)))
            (reset-state
             . ,(lambda () (setq state-reset-p t)))
            (set-namespace
             . ,(lambda (ns) (setq namespace-state ns))))))

    (kubernetes-overview--initialize-client props "updated-namespace")
    (should state-reset-p)
    (should client-stopped-p)
    (should client-started-p)
    (should client-running-p)
    (should (equal namespace-state "updated-namespace"))))

(ert-deftest kubernetes-overview-test--initialize-client--namespace-unchanged ()
  (let* ((client-started-p)
         (client-stopped-p)
         (state-reset-p)
         (client-running-p t)
         (namespace-state "initial-namespace")
         (props
          `((get-client-process
             . ,(lambda () client-running-p))
            (get-namespace
             . ,(lambda () namespace-state))
            (start-client
             . ,(lambda ()
                  (setq client-started-p t)
                  (setq client-running-p t)))
            (stop-client
             . ,(lambda ()
                  (setq client-stopped-p t)
                  (setq client-running-p nil)))
            (reset-state
             . ,(lambda () (setq state-reset-p t)))
            (set-namespace
             . ,(lambda (ns) (setq namespace-state ns))))))

    (kubernetes-overview--initialize-client props namespace-state)
    (should-not state-reset-p)
    (should-not client-stopped-p)
    (should-not client-started-p)
    (should client-running-p)
    (should (equal namespace-state "initial-namespace"))))

(ert-deftest kubernetes-overview-test--initialize-client--client-not-running ()
  (let* ((client-started-p)
         (client-stopped-p)
         (state-reset-p)
         (client-running-p)
         (namespace-state "initial-namespace")
         (props
          `((get-client-process
             . ,(lambda () client-running-p))
            (get-namespace
             . ,(lambda () namespace-state))
            (start-client
             . ,(lambda ()
                  (setq client-started-p t)
                  (setq client-running-p t)))
            (stop-client
             . ,(lambda ()
                  (setq client-stopped-p t)
                  (setq client-running-p nil)))
            (reset-state
             . ,(lambda () (setq state-reset-p t)))
            (set-namespace
             . ,(lambda (ns) (setq namespace-state ns))))))

    (kubernetes-overview--initialize-client props "updated-namespace")
    (should-not client-stopped-p)
    (should state-reset-p)
    (should client-started-p)
    (should client-running-p)
    (should (equal namespace-state "updated-namespace"))))


;; Redrawing

(ert-deftest kubernetes-overview-test--redraw--raises-error-if-no-buffer ()
  (should-error (kubernetes-overview-redraw nil nil)))

(ert-deftest kubernetes-overview-test--redraw--suppressed-when-region-is-active ()
  (with-temp-buffer
    (let* ((buffer-redrawn-p)
           (props `((ast-render
                     . ,(lambda (&rest _) (setq buffer-redrawn-p t)))
                    (region-active-p
                     . (lambda () t))
                    (get-state
                     . (lambda () (error "Unexpeced: get-state"))))))
      (kubernetes-overview-redraw (current-buffer) props)
      (should-not buffer-redrawn-p))))

(ert-deftest kubernetes-overview-test--redraw--draws-overview ()
  (with-temp-buffer
    (let* ((component)
           (props `((ast-render
                     . ,(lambda (_ c) (setq component c)))
                    (region-active-p
                     . (lambda () nil))
                    (get-state
                     . (lambda () nil)))))
      (kubernetes-overview-redraw (current-buffer) props)
      (should (equal 'overview (car component))))))


;; Killing overview

(ert-deftest kubernetes-overview-test--tear-down-overview ()
  (let* ((client-stopped-p)
         (state-cleared-p)
         (props
          `((stop-client . ,(lambda () (setq client-stopped-p t)))
            (clear-state . ,(lambda () (setq state-cleared-p t))))))
    (kubernetes-overview--tear-down-overview props)
    (should client-stopped-p)
    (should state-cleared-p)))


;; Setting up the overview buffer

(ert-deftest kubernetes-overview-test--setup-buffer ()
  (let* ((kubernetes-overview-buffer (generate-new-buffer-name " test setup buffer"))
         (buffer-redrawn-p)
         (props `((kubernetes-overview-redraw
                   . ,(lambda (&rest _) (setq buffer-redrawn-p t))))))
    (with-current-buffer (kubernetes-overview--setup-buffer props)
      (should buffer-redrawn-p)
      (should (member #'kubernetes-overview--tear-down-overview kill-buffer-hook)))))


(provide 'kubernetes-overview-test)

;;; kubernetes-overview-test.el ends here
