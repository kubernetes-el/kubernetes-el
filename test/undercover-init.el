(when (require 'undercover nil t)
  (with-no-warnings
    (undercover "*.el"
                (:exclude "kubernetes-evil.el")
                (:report-format 'lcov)
                (:send-report nil))))
