;;; kubernetes-process.el --- Process management.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'subr-x)
(require 'term)

(require 'kubernetes-ast)

(defun kubernetes--term-buffer-start (bufname command args)
  ;; Kill existing process.
  (when-let ((existing (get-buffer bufname))
             (proc (get-buffer-process existing)))
    (kubernetes--kill-process-quietly proc))

  (let ((buf (get-buffer-create bufname)))
    (with-current-buffer buf
      (erase-buffer)
      (buffer-disable-undo)
      (term-mode)
      (goto-char (point-min))
      (let ((time-str (format "Session started at %s" (substring (current-time-string) 0 19)))
            (command-str (format "%s %s" command (string-join args " "))))
        (kubernetes--eval-ast
         `((line ,(propertize time-str 'face 'magit-dimmed))
           (padding)
           (line ,(propertize command-str 'face 'magit-dimmed))
           (padding))))

      (term-exec (current-buffer) "kuberenetes-term" command nil args)
      (let ((proc (get-buffer-process (current-buffer))))
        (set-process-query-on-exit-flag proc nil)
        (term-char-mode)
        (add-hook 'kill-buffer-hook (lambda ()
                                      (when-let (win (get-buffer-window buf))
                                        (quit-window nil win)))
                  nil t)))

    buf))

(defun kubernetes--process-buffer-start (bufname setup-fn command args &optional process-filter)
  (let ((buf (get-buffer-create bufname)))
    (buffer-disable-undo buf)

    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall setup-fn)
        (let ((time-str (format "Process started at %s" (substring (current-time-string) 0 19)))
              (command-str (format "%s %s" command (string-join args " "))))
          (kubernetes--eval-ast
           `((line ,(propertize time-str 'face 'magit-dimmed))
             (padding)
             (line ,(propertize command-str 'face 'magit-dimmed))
             (padding))))))

    (let ((proc (apply #'start-process "kubernetes-exec" buf command args)))
      (when process-filter
        (set-process-filter proc process-filter))
      (set-process-query-on-exit-flag proc nil))
    buf))

(defun kubernetes--kill-process-quietly (proc &optional _signal)
  (when proc
    (set-process-sentinel proc nil)
    (set-process-query-on-exit-flag proc nil)
    (let ((kill-buffer-query-functions nil)
          (buf (process-buffer proc)))
      (ignore-errors (kill-process proc))
      (ignore-errors (delete-process proc))
      (ignore-errors (kill-buffer buf)))))


(provide 'kubernetes-process)

;;; kubernetes-process.el ends here
