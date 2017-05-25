;;; kubernetes-mode-test.el --- Tests for kubernetes-mode.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (unless (bound-and-true-p test-helper-loaded)
    (load (concat default-directory "test-helper.el"))))

(require 'kubernetes-mode)

(ert-deftest kubernetes-mode-test--configures-buffers ()
  (with-temp-buffer
    (kubernetes-mode)
    (should buffer-read-only)))

;; Disables line numbers

(defvar global-linum-mode)
(defvar global-nlinum-mode)

(ert-deftest kubernetes-mode-test--disable-linum-modes ()
  (with-temp-buffer
    (let ((global-linum-mode t)
          (global-nlinum-mode t)

          (kubernetes-mode-props
           `((linum-mode . ,(lambda (n)
                              (should (equal -1 n))))
             (nlinum-mode . ,(lambda (n)
                               (should (equal -1 n)))))))
      (kubernetes-mode))))

(provide 'kubernetes-mode-test)

;;; kubernetes-mode-test.el ends here
