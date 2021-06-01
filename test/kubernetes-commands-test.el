;;; kubernetes-commands-test.el --- Test interactive commands -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun make-buffer-with-mode (name mode)
  "Make buffer named NAME with major mode MODE."
  (with-current-buffer (get-buffer-create name)
    (funcall mode)))

(ert-deftest kubernetes-commands--kubernetes-kill-buffers ()
  (let ((buffers-modes '(("foo" kubernetes-mode)
                         ("bar" kubernetes-overview-mode)
                         ("baz" kubernetes-log-line-mode))))
    (dolist (buffer-mode buffers-modes)
      (apply #'make-buffer-with-mode buffer-mode))
    (cl-letf (((symbol-function 'message) 'ignore)
              ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (kubernetes-kill-buffers))
    (-map (lambda (buffer-mode)
            (should (equal nil (get-buffer (car buffer-mode)))))
          buffers-modes)))

;;; kubernetes-commands-test.el ends here
