;;; kubernetes-yaml-test.el --- Test rendering of YAML in general -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(ert-deftest kubernetes-yaml-test--yaml-render ()
  (let* ((json "{\"args\": [\"--foo\", \"--bar\"], \"more\": 1}")
         (parsed-json (json-read-from-string json)))
    (with-temp-buffer
      (save-excursion (kubernetes-ast-eval (kubernetes-yaml-render
                                            parsed-json)))
      ;; white space at end of line is deliberate
      (should (equal "args: 
  - --foo
  - --bar
more: 1

" (buffer-string))))))

(ert-deftest kubernetes-yaml-test--yaml-render--list ()
  (let* ((json "[\"--foo\", \"--bar\"]")
         (parsed-json (json-read-from-string json))
         (rendered (kubernetes-yaml-render parsed-json)))
    (with-temp-buffer
      (save-excursion (kubernetes-ast-eval rendered))
      (should (equal "- --foo
- --bar

" (buffer-string))))))

;;; kubernetes-yaml-test.el ends here
