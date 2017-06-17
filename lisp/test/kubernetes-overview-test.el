;;; kubernetes-overview-test.el --- Tests for kubernetes-overview.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (unless (bound-and-true-p test-helper-loaded)
    (load (concat default-directory "test-helper.el"))))

(require 'kubernetes-overview)


;; Handling updates from the backend

(ert-deftest kubernetes-overview-test--update-handler--no-redraw-if-overview-killed ()
  (let* ((kubernetes-redraw-on-updates t)
         (props
          `((buffer-live-p
             . (lambda (_) nil))
            (kubernetes-overview--redraw
             . (lambda (&rest _) (error "Unexpected: kubernetes-overview--redraw")))
            (set-overview-populated-p
             . (lambda (_) (error "Unexpected: overview-populated-p")))
            (overview-populated-p
             . (lambda () (error "Unexpected: overview-populated-p")))))

         (handler (kubernetes-overview--mk-client-message-handler
                   props
                   (current-buffer))))

    (should-not (funcall handler nil))))

(ert-deftest kubernetes-overview-test--update-handler--no-redraw-if-overview-populated-and-redraws-disabled ()
  (let* ((kubernetes-redraw-on-updates nil)
         (props
          `((buffer-live-p
             . (lambda (_) t))
            (overview-populated-p
             . (lambda () t))
            (kubernetes-overview--redraw
             . (lambda (&rest _) (error "Unexpected: kubernetes-overview--redraw")))
            (set-overview-populated-p
             . (lambda (_) (error "Unexpected: overview-populated-p")))))

         (handler (kubernetes-overview--mk-client-message-handler
                   props
                   (current-buffer))))

    (should-not (funcall handler nil))))

(ert-deftest kubernetes-overview-test--update-handler--redraws-if-updates-enabled ()
  (let* ((kubernetes-redraw-on-updates t)
         (buffer-redrawn-p nil)
         (state-updated-p nil)
         (props
          `((buffer-live-p
             . (lambda (_) t))
            (overview-populated-p
             . (lambda () t))
            (kubernetes-overview--redraw
             . ,(lambda (&rest _) (setq buffer-redrawn-p t)))
            (set-overview-populated-p
             . ,(lambda (value) (setq state-updated-p value)))))

         (handler (kubernetes-overview--mk-client-message-handler
                   props
                   (current-buffer))))

    (should (funcall handler nil))
    (should buffer-redrawn-p)
    (should state-updated-p)))

(ert-deftest kubernetes-overview-test--update-handler--initial-redraw-on-updates-disabled ()
  (let* ((kubernetes-redraw-on-updates nil)
         (buffer-redrawn-p nil)
         (state-updated-p nil)
         (props
          `((buffer-live-p
             . (lambda (_) t))
            (overview-populated-p
             . (lambda () nil))
            (kubernetes-overview--redraw
             . ,(lambda (&rest _) (setq buffer-redrawn-p t)))
            (set-overview-populated-p
             . ,(lambda (value) (setq state-updated-p value)))))

         (handler (kubernetes-overview--mk-client-message-handler
                   props
                   (current-buffer))))

    (should (funcall handler nil))
    (should buffer-redrawn-p)
    (should state-updated-p)))

(provide 'kubernetes-overview-test)

;;; kubernetes-overview-test.el ends here
