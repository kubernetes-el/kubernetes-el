;;; kubernetes-overview-test.el --- Tests for kubernetes-overview.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'kubernetes-overview)


;; Uses state to determine which sections to draw

(ert-deftest kubernetes-overview-test--renders-selected-sections-from-state ()
  (test-helper-with-empty-state
    (kubernetes-state-update-overview-sections '(configmaps secrets))
    (kubernetes-ast-eval (kubernetes-overview-render (kubernetes-state)))

    (let ((expected (with-temp-buffer
                      (kubernetes-ast-eval `(section (root nil)
                                                     ,(kubernetes-errors-render (kubernetes-state))
                                                     ,(kubernetes-configmaps-render (kubernetes-state))
                                                     ,(kubernetes-secrets-render (kubernetes-state))))
                      (substring-no-properties (buffer-string)))))
      (should (equal expected (substring-no-properties (buffer-string)))))))

(provide 'kubernetes-overview-test)

;;; kubernetes-overview-test.el ends here
