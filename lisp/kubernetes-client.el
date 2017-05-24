;;; kubernetes-client.el --- Integration with custom Go client.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'kubernetes-props)

(defgroup kubernetes-client nil
  "Polling subprocess."
  :group 'kubernetes
  :prefix "kubernetes-client-")

(defcustom kubernetes-client-executable "emacs-k8s"
  "The name of the kubernetes-el client program.

It should either be an absolute path to the program, or the name
of a program in the Emacs `exec-path'."
  :group 'kubernetes-client
  :type 'string)

(defcustom kubernetes-client-polling-interval 10
  "The polling interval in seconds for the kubernetes-el client program."
  :group 'kubernetes-client
  :type 'integer)

(defconst kubernetes-client-props
  '((message . message)
    (make-process . make-process)
    (process-buffer . process-buffer)
    (process-mark . process-mark)
    (handle-line . kubernetes-state-handle-client-line)
    (get-namespace . kubernetes-state-namespace)
    (get-client-process . kubernetes-state-client-process)
    (set-client-process . kubernetes-state-set-client-process))
  "Functions to inject for isolation and testing.")

(defun kubernetes-client--make-line-handler-filter (props)
  (kubernetes-props-bind ([process-buffer process-mark handle-line] props)
    (lambda (process str)
      (let ((buf (funcall process-buffer process))
            (marker (funcall process-mark process)))

        (when (buffer-live-p buf)
          (with-current-buffer buf
            (save-excursion
              (goto-char marker)

              ;; Insert chars until a newline is encountered, at which point the
              ;; buffer is flushed and the line is passed to the handler routine.
              (let ((i 0))
                (while (< i (length str))
                  (let ((ch (aref str i)))
                    (cond ((= ch ?\n)
                           (funcall handle-line (buffer-string))
                           (erase-buffer))
                          (t
                           (insert ch))))

                  (setq i (1+ i))))

              (set-marker marker (point)))))))))

(defun kubernetes-client-start (&optional props)
  "Start the kubernetes-el background process.

PROPS is an alist of functions to be injected."
       (interactive (list kubernetes-client-props))
       (let ((props (or props kubernetes-client-props)))
         (kubernetes-props-bind ([get-client-process set-client-process get-namespace message make-process]
                                 props)
           (if (funcall get-client-process)
               (user-error "Kubernetes client already running")
             (funcall message "Starting Kubernetes client")
             (let* ((buf (generate-new-buffer " emacs-k8s"))
                    (err-buf (generate-new-buffer " emacs-k8s-err"))
                    (command
                     (list kubernetes-client-executable
                           "-namespace" (funcall get-namespace)
                           "-interval" (number-to-string kubernetes-client-polling-interval)))

                    (process
                     (funcall make-process
                              :name "emacs-k8s"
                              :buffer buf
                              :stderr err-buf
                              :command command
                              :noquery t
                              :filter (kubernetes-client--make-line-handler-filter props)
                              :sentinel
                              (lambda (process _status)
                                (let ((kill-buffer-query-functions nil)
                                      (exit-code (process-exit-status process)))
                                  (cond
                                   ;; Shut down gracefully.
                                   ((= 0 exit-code))
                                   ;; Killed by Emacs.
                                   ((= 9 exit-code))
                                   ;; Unknown failure.
                                   (t
                                    (with-current-buffer err-buf
                                      (funcall message "Kubernetes client exited unexpectedly: %s" (buffer-string)))))
                                  (kill-buffer buf)
                                  (ignore-errors (kill-buffer err-buf)))))))

               (funcall set-client-process process)

               ;; Clean up process if the process buffer is killed.
               (with-current-buffer buf
                 (add-hook 'kill-buffer-hook #'kubernetes-client-stop nil t))

               process)))))

(defun kubernetes-client-stop (&optional props)
  "Stop the kubernetes-el background process.

PROPS is an alist of functions to be injected."
       (interactive (list kubernetes-client-props))
       (let ((props (or props kubernetes-client-props)))
         (kubernetes-props-bind ([get-client-process set-client-process message]
                                 props)
           (let ((process (funcall get-client-process)))
             (cond
              (process
               (set-process-query-on-exit-flag process nil)
               (let ((kill-buffer-query-functions nil)
                     (buf (process-buffer process)))
                 (ignore-errors (kill-process process))
                 (ignore-errors (delete-process process))
                 (ignore-errors (kill-buffer buf)))
               (funcall set-client-process nil)
               (funcall message "Kubernetes client stopped"))
              (t
               (user-error "Kubernetes client not running")))))))

(provide 'kubernetes-client)

;;; kubernetes-client.el ends here
