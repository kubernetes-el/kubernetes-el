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


;; rejection

(ert-deftest eval-ast--rejects-invalid-ast ()
  (let ((ast '(foo "bar")))
    (with-temp-buffer
      (should-error (kubernetes--eval-ast ast)))))


;; padding

(ert-deftest eval-ast--inserts-newline-for-padding ()
  (let ((ast '(padding)))
    (with-temp-buffer
      (kubernetes--eval-ast ast)
      (should (equal "\n" (buffer-string))))))


;; line

(ert-deftest eval-ast--inserts-newlines-for-empty-strings ()
  (let ((ast '(line "")))
    (with-temp-buffer
      (kubernetes--eval-ast ast)
      (should (equal "\n" (buffer-string))))))

(ert-deftest eval-ast--inserts-strings ()
  (let ((ast '(line "foo")))
    (with-temp-buffer
      (kubernetes--eval-ast ast)
      (should (equal "foo\n" (buffer-string))))))


;; propertize

(ert-deftest eval-ast--propertizes-regions ()
  (let ((ast '(propertize (face error) (line "foo"))))
    (with-temp-buffer
      (kubernetes--eval-ast ast)
      (should (equal (propertize "foo\n" 'face 'error) (buffer-string))))))


;; sequence

(ert-deftest eval-ast--sequencing-actions ()
  (let ((ast '((line "foo")
               (line "bar"))))
    (with-temp-buffer
      (save-excursion (kubernetes--eval-ast ast))
      (should (equal "foo\nbar\n" (buffer-string))))))


;; heading

(ert-deftest eval-ast--inserts-headings ()
  (let ((ast '(heading "hello")))
    (with-temp-buffer
      (save-excursion
        (magit-insert-section (test)
          (kubernetes--eval-ast ast)))
      (should (equal "hello\n" (substring-no-properties (buffer-string))))
      (should (equal 'magit-section-heading (face-at-point))))))

(ert-deftest eval-ast--inserting-heading-raises-error-outside-section ()
  (let ((ast '(heading "hello")))
    (with-temp-buffer
      (should-error (kubernetes--eval-ast ast)))))


;; section

(ert-deftest eval-ast--inserting-sections ()
  (let ((ast '(section (test nil)
                       (line "foo"))))
    (with-temp-buffer
      (save-excursion (kubernetes--eval-ast ast))
      (should (equal "foo\n" (substring-no-properties (buffer-string))))
      (should (magit-current-section))
      (should (equal 'test (magit-section-type (magit-current-section))))
      (should (not (magit-section-hidden (magit-current-section)))))))


;; indent

(ert-deftest eval-ast--indents-ast ()
  (let ((ast '(indent (line "hello"))))
    (with-temp-buffer
      (save-excursion (kubernetes--eval-ast ast))
      (should (equal "  hello\n" (substring-no-properties (buffer-string)))))))

(ert-deftest eval-ast--indentation-padding-lacks-properties-directly-set-on-string ()
  (let ((ast `(indent (line ,(propertize "hello" 'face 'font-lock-warning-face)))))
    (with-temp-buffer
      (save-excursion (kubernetes--eval-ast ast))
      (should (equal "  " (substring-no-properties (buffer-string) 0 2)))
      (should (not (text-property-any 0 kubernetes--render-indentation-width
                                      'face 'font-lock-warning-face
                                      (buffer-string)))))))

(ert-deftest eval-ast--indentation-padding-has-ast-declared-properties ()
  (let ((ast '(indent
               (propertize (face font-lock-warning-face)
                           (line "hello")))))
    (with-temp-buffer
      (save-excursion (kubernetes--eval-ast ast))
      (should (equal "  " (substring-no-properties (buffer-string) 0 2)))
      (should (text-property-any 0 kubernetes--render-indentation-width
                                 'face 'font-lock-warning-face
                                 (buffer-string))))))


;; key-value

(ert-deftest eval-ast--key-value-pairs ()
  (let ((ast '(key-value 10 "Key" "Value")))
    (with-temp-buffer
      (save-excursion (kubernetes--eval-ast ast))
      (should (equal (format "%-10s%s\n" "Key:" "Value") (substring-no-properties (buffer-string)))))))


;; nav-prop

(ert-deftest eval-ast--nav-prop ()
  (let ((ast '(nav-prop (:test "nav")
                        (line "Test"))))
    (with-temp-buffer
      (save-excursion (kubernetes--eval-ast ast))
      (should (equal "Test\n" (buffer-string)))
      (should (equal (list 'kubernetes-nav '(:test "nav"))
                     (text-properties-at (point-min) (buffer-string)))))))

(ert-deftest eval-ast--nav-prop-finishes-at-end-of-line ()
  (let ((ast '(nav-prop (:test "nav")
                        (line "Test"))))
    (with-temp-buffer
      (save-excursion (kubernetes--eval-ast ast))
      (let ((end (1- (line-end-position))))
        (should (equal (list 'kubernetes-nav '(:test "nav"))
                       (text-properties-at end (buffer-string))))))))


;; copy-prop

(ert-deftest eval-ast--copy-prop ()
  (let ((ast '(copy-prop "foo" (line "Test"))))
    (with-temp-buffer
      (save-excursion (kubernetes--eval-ast ast))
      (should (equal "Test\n" (buffer-string)))
      (should (equal (list 'kubernetes-copy "foo")
                     (text-properties-at (point-min) (buffer-string)))))))

(ert-deftest eval-ast--copy-prop-finishes-at-end-of-line ()
  (let ((ast '(copy-prop "foo" (line "Test"))))
    (with-temp-buffer
      (save-excursion (kubernetes--eval-ast ast))
      (let ((end (1- (line-end-position))))
        (should (equal (list 'kubernetes-copy "foo")
                       (text-properties-at end (buffer-string))))))))

(ert-deftest eval-ast--copy-prop-error-if-copy-value-not-a-string ()
  (let ((ast '(copy-prop 1 (line "Test"))))
    (with-temp-buffer
      (should-error (kubernetes--eval-ast ast)))))


;; mark-for-delete

(ert-deftest eval-ast--mark-for-delete-no-indentation ()
  (let ((ast '(mark-for-delete (line "Test"))))
    (with-temp-buffer
      (save-excursion (kubernetes--eval-ast ast))
      (should (equal "D Test\n" (buffer-string)))
      (should (equal '(face kubernetes-delete-mark)
                     (text-properties-at 0 (buffer-string))))
      (should (not (text-properties-at 1 (buffer-string)))))))

(ert-deftest eval-ast--mark-for-delete-with-indentation ()
  (let ((ast '(mark-for-delete (line "  Test"))))
    (with-temp-buffer
      (save-excursion (kubernetes--eval-ast ast))
      (should (equal "D Test\n" (buffer-string)))
      (should (equal '(face kubernetes-delete-mark)
                     (text-properties-at 0 (buffer-string))))
      (should (not (text-properties-at 1 (buffer-string)))))))

(ert-deftest eval-ast--mark-for-delete-multiple-lines ()
  (let ((ast '((mark-for-delete (line "foo")
                                (line "bar")
                                (line "baz"))
               (line "frotz"))))
    (with-temp-buffer
      (save-excursion (kubernetes--eval-ast ast))
      (should (equal "D foo\nD bar\nD baz\nfrotz\n" (buffer-string))))))


;;; eval-ast-test.el ends here
