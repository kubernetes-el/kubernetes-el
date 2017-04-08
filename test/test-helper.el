;;; test-helper.el --- Setup run before tests.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (require 'undercover nil t)
  (undercover "*.el"
              (:exclude "kubernetes-evil.el")
              (:send-report nil)))

(require 'kubernetes)

;;; test-helper.el ends here
