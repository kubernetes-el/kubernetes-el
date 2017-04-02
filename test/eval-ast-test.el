;;; eval-ast-test.el --- Tests for rending using an AST.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-and-compile
  (require 'f)

  (defvar project-root
    (locate-dominating-file default-directory ".git"))

  (defvar this-directory
    (f-join project-root "test")))

(require 'kubernetes (f-join project-root "kubernetes.el"))

(ert-deftest eval-ast--rejects-invalid-ast ()
  (let ((ast '(foo "bar")))
    (with-temp-buffer
      (should-error (kubernetes--eval-ast ast)))))

(ert-deftest eval-ast--inserts-2-newlines-for-padding ()
  (let ((ast '(padding)))
    (with-temp-buffer
      (kubernetes--eval-ast ast)
      (should (equal "\n\n" (buffer-string))))))

(ert-deftest eval-ast--inserts-newlines-for-empty-strings ()
  (let ((ast '(line . "")))
    (with-temp-buffer
      (kubernetes--eval-ast ast)
      (should (equal "\n" (buffer-string))))))

(ert-deftest eval-ast--inserts-strings ()
  (let ((ast '(line . "foo")))
    (with-temp-buffer
      (kubernetes--eval-ast ast)
      (should (equal "foo\n" (buffer-string))))))

(ert-deftest eval-ast--sequencing-actions ()
  (let ((ast '((line . "foo")
               (line . "bar"))))
    (with-temp-buffer
      (save-excursion (kubernetes--eval-ast ast))
      (should (equal "foo\nbar\n" (buffer-string))))))

(ert-deftest eval-ast--inserts-headings ()
  (let ((ast '(heading . "hello")))
    (with-temp-buffer
      (save-excursion
        (magit-insert-section (test)
          (kubernetes--eval-ast ast)))
      (should (equal "hello\n" (substring-no-properties (buffer-string))))
      (should (equal 'magit-section-heading (face-at-point))))))

(ert-deftest eval-ast--inserting-heading-raises-error-outside-section ()
  (let ((ast '(heading . "hello")))
    (with-temp-buffer
      (should-error (kubernetes--eval-ast ast)))))

(ert-deftest eval-ast--inserting-sections-with-symbol-type ()
  (let ((ast '(section (symbol test nil)
                       (line . "foo"))))
    (with-temp-buffer
      (save-excursion (kubernetes--eval-ast ast))
      (should (equal "foo\n" (substring-no-properties (buffer-string))))
      (should (magit-current-section))
      (should (equal 'test (magit-section-type (magit-current-section))))
      (should (not (magit-section-hidden (magit-current-section)))))))

(ert-deftest eval-ast--inserting-sections-with-eval-type ()
  (let ((ast '(section (eval (intern "hello") nil)
                       (line . "foo"))))
    (with-temp-buffer
      (save-excursion (kubernetes--eval-ast ast))
      (should (equal "foo\n" (substring-no-properties (buffer-string))))
      (should (equal 'hello (magit-section-type (magit-current-section))))
      (should (not (magit-section-hidden (magit-current-section)))))))

;;; eval-ast-test.el ends here
