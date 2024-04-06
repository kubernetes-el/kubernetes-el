;;; undercover-init.el --- Initialize undercover. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (require 'undercover nil t)
  (with-no-warnings
    (undercover "*.el"
                (:exclude "kubernetes-evil.el")
                (:report-file "./coverage/lcov-buttercup.info")
                (:report-format 'lcov)
                (:send-report nil))))
