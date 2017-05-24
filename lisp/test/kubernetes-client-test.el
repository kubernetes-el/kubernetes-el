;;; kubernetes-client-test.el --- Tests for kubernetes-client.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(eval-when-compile
  (unless (bound-and-true-p test-helper-loaded)
    (load (concat default-directory "test-helper.el"))))

(require 'kubernetes-client)


;; Starting the client process

(ert-deftest kubernetes-client-test--start--raises-error-if-already-running ()
  (let ((props `((message . ignore)
                 (get-client-process . (lambda () t))
                 (set-client-process) (get-namespace) (make-process))))
    (should-error (kubernetes-client-start props)
                  :type 'user-error)))

(ert-deftest kubernetes-client-test--start--starts-process-if-not-running ()
  (let* ((process-set-in-state)
         (process-started)
         (command)
         (kubernetes-client-polling-interval 5)
         (props
          `((message . ignore)
            (make-process . ,(lambda (&rest args)
                               (setq command (plist-get args :command))
                               (setq process-started t)))
            (get-namespace . ,(lambda () "foo"))
            (get-client-process . ,(lambda () nil))
            (set-client-process . ,(lambda (proc)
                                     (setq process-set-in-state proc)))
            (process-buffer) (process-mark) (handle-line)))
         (process (kubernetes-client-start props)))

    (should process-started)
    (should process-set-in-state)
    (should (equal command (list kubernetes-client-executable "-namespace" "foo" "-interval" "5")))
    (should (equal process-set-in-state process))))


;; Stopping the client process

(ert-deftest kubernetes-client-test--stop--raises-error-if-not-running ()
  (let ((props `((message . ignore)
                 (get-client-process . (lambda () nil))
                 (set-client-process))))
    (should-error (kubernetes-client-stop props)
                  :type 'user-error)))

(ert-deftest kubernetes-client-test--stop--stops-processes-if-running ()
  (let* ((buf (generate-new-buffer " test"))
         (process (start-process-shell-command " test" buf "sleep 1000")))
    (should (process-live-p process))

    (let* ((process-deleted-from-state)
           (props
            `((message . ignore)
              (get-client-process . ,(lambda () process))
              (set-client-process . ,(lambda (_)
                                       (setq process-deleted-from-state t))))))

      (kubernetes-client-stop props)

      (should process-deleted-from-state)
      (should-not (buffer-live-p buf))
      (should-not (process-live-p process)))))


;; Processing client output

(ert-deftest kubernetes-client-test--process-filter--handles-lines ()
  (let* ((processed-lines)
         (buf (generate-new-buffer " test"))
         (marker (set-marker (make-marker) 0 buf))
         (props
          `((process-buffer . ,(lambda (_) buf))
            (process-mark . ,(lambda (_) marker))
            (handle-line . ,(lambda (line)
                              (let ((updated (-snoc processed-lines line)))
                                (setq processed-lines updated))))))
         (process-filter
          (kubernetes-client--make-line-handler-filter props)))

    (funcall process-filter nil "foo\nb")
    (funcall process-filter nil "ar\nbaz\n")

    (should (equal '("foo" "bar" "baz") processed-lines))))


(provide 'kubernetes-client-test)

;;; kubernetes-client-test.el ends here
