;;; kubernetes-props-test.el --- Tests for kubernetes-props.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (unless (bound-and-true-p test-helper-loaded)
    (load (concat default-directory "test-helper.el"))))

(require 'kubernetes-props)

(ert-deftest kubernetes-props-test--bind--validates-args ()
  (should-error (eval '(kubernetes-props-bind)))
  (should-error (eval '(kubernetes-props-bind 'foo)))
  (should-error (eval '(kubernetes-props-bind ())))
  (should-error (eval '(kubernetes-props-bind ((foo bar) ()))))
  (should-error (eval '(kubernetes-props-bind ([1 2] ()))))
  (should-error (eval '(kubernetes-props-bind ([1 2] 'baz))))
  (should-error (eval '(kubernetes-props-bind ([] 'baz))))
  (should-error (eval '(kubernetes-props-bind ([f] nil)))))

(ert-deftest kubernetes-props-test--bind--binds-lexical-function-definitons ()
  (let* ((props '((message . (lambda () 1))
                  (format . (lambda () 2))
                  (foo . (lambda () 3)))))
    (should (equal 6
                   (kubernetes-props-bind ([message format foo] props)
                     (+ (message) (format) (foo)))))))

(provide 'kubernetes-props-test)

;;; kubernetes-props-test.el ends here
