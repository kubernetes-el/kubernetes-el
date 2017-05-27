;;; kubernetes-ast-test.el --- Tests for kubernetes-ast.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (unless (bound-and-true-p test-helper-loaded)
    (load (concat default-directory "test-helper.el"))))

(require 'kubernetes-ast)
(require 'magit-section)
(require 'subr-x)


;; rejection

(ert-deftest kubernetes-ast-test--rejects-invalid-ast ()
  (let* ((props '((message . ignore)))
         (ast [hello]))
    (with-temp-buffer
      (should-error (kubernetes-ast-eval ast nil props)))))

(ert-deftest kubernetes-ast-test--rejects-undefined-component ()
  (let ((ast '(foo "bar")))
    (with-temp-buffer
      (should-error (kubernetes-ast-eval ast)))))


;; padding

(ert-deftest kubernetes-ast-test--inserts-newline-for-padding ()
  (let ((ast '(padding)))
    (with-temp-buffer
      (kubernetes-ast-eval ast)
      (should (equal "\n" (buffer-string))))))

(ert-deftest kubernetes-ast-test--padding-errors-if-applied-to-arguments ()
  (let ((ast '(padding foo)))
    (with-temp-buffer
      (should-error (kubernetes-ast-eval ast)))))


;; line

(ert-deftest kubernetes-ast-test--inserts-newlines-for-empty-strings ()
  (let ((ast '(line "")))
    (with-temp-buffer
      (kubernetes-ast-eval ast)
      (should (equal "\n" (buffer-string))))))

(ert-deftest kubernetes-ast-test--inserts-strings ()
  (let ((ast '(line "foo")))
    (with-temp-buffer
      (kubernetes-ast-eval ast)
      (should (equal "foo\n" (buffer-string))))))


;; list

(ert-deftest kubernetes-ast-test--lists ()
  (let ((ast '(list (line "foo") (line "bar") (line "baz")))
        (expected (string-trim-left "
- foo
- bar
- baz
")))
    (with-temp-buffer
      (kubernetes-ast-eval ast)
      (should (equal expected (buffer-string))))))

(ert-deftest kubernetes-ast-test--lists-with-multiple-lines ()
  (let ((ast '(list ((line "foo") (line "bar"))
                    ((line "foo") (line "  bar"))
                    ("foo" "bar")))
        (expected (string-trim-left "
- foo
  bar
- foo
    bar
- foobar")))
    (with-temp-buffer
      (kubernetes-ast-eval ast)
      (should (equal expected (buffer-string))))))

(ert-deftest kubernetes-ast-test--lists-with-indentation ()
  (let ((ast '(list (line "foo")
                    (line "bar")
                    (list (line "foo")
                          (line "bar"))
                    (line "baz")))
        (expected (string-trim-left "
- foo
- bar
  - foo
  - bar
- baz
")))
    (with-temp-buffer
      (kubernetes-ast-eval ast)
      (should (equal expected (buffer-string))))))


;; propertize

(ert-deftest kubernetes-ast-test--propertizes-regions ()
  (let ((ast '(propertize (face error) (line "foo"))))
    (with-temp-buffer
      (kubernetes-ast-eval ast)
      (should (equal (propertize "foo\n" 'face 'error) (buffer-string))))))


;; sequence

(ert-deftest kubernetes-ast-test--sequencing-actions ()
  (let ((ast '((line "foo")
               (line "bar"))))
    (with-temp-buffer
      (save-excursion (kubernetes-ast-eval ast))
      (should (equal "foo\nbar\n" (buffer-string))))))


;; heading

(ert-deftest kubernetes-ast-test--inserts-headings ()
  (let ((ast '(heading "hello")))
    (with-temp-buffer
      (save-excursion
        (magit-insert-section (test)
          (kubernetes-ast-eval ast)))
      (should (equal "hello\n" (substring-no-properties (buffer-string))))
      (should (equal 'magit-section-heading (face-at-point))))))

(ert-deftest kubernetes-ast-test--inserting-heading-raises-error-outside-section ()
  (let ((ast '(heading "hello")))
    (with-temp-buffer
      (should-error (kubernetes-ast-eval ast)))))


;; section

(ert-deftest kubernetes-ast-test--inserting-sections ()
  (let ((ast '(section (root nil)
                       (section (test-line nil)
                                (line "foo")))))
    (with-temp-buffer
      (save-excursion (kubernetes-ast-eval ast))
      (should (equal "foo\n" (substring-no-properties (buffer-string))))
      (should (magit-current-section))
      (should (equal 'test-line (magit-section-type (magit-current-section))))
      (should (not (magit-section-hidden (magit-current-section)))))))


;; indent

(ert-deftest kubernetes-ast-test--indents-ast ()
  (let ((ast '(indent (line "hello"))))
    (with-temp-buffer
      (save-excursion (kubernetes-ast-eval ast))
      (should (equal "  hello\n" (substring-no-properties (buffer-string)))))))

(ert-deftest kubernetes-ast-test--indentation-padding-lacks-properties-directly-set-on-string ()
  (let ((ast `(indent (line ,(propertize "hello" 'face 'font-lock-warning-face)))))
    (with-temp-buffer
      (save-excursion (kubernetes-ast-eval ast))
      (should (equal "  " (substring-no-properties (buffer-string) 0 2)))
      (should (not (text-property-any 0 kubernetes-ast--indentation-width
                                      'face 'font-lock-warning-face
                                      (buffer-string)))))))

(ert-deftest kubernetes-ast-test--indentation-padding-has-ast-declared-properties ()
  (let ((ast '(indent
               (propertize (face font-lock-warning-face)
                           (line "hello")))))
    (with-temp-buffer
      (save-excursion (kubernetes-ast-eval ast))
      (should (equal "  " (substring-no-properties (buffer-string) 0 2)))
      (should (text-property-any 0 kubernetes-ast--indentation-width
                                 'face 'font-lock-warning-face
                                 (buffer-string))))))


;; key-value

(ert-deftest kubernetes-ast-test--key-value-pairs ()
  (let ((ast '(key-value 10 "Key" "Value")))
    (with-temp-buffer
      (save-excursion (kubernetes-ast-eval ast))
      (should (equal (format "%-10s%s\n" "Key:" "Value") (substring-no-properties (buffer-string)))))))

(ert-deftest kubernetes-ast-test--key-value--validates-width-as-nat ()
  (with-temp-buffer
    (should-assert (kubernetes-ast-eval '(key-value "10" "Key" "Value")))
    (should-assert (kubernetes-ast-eval '(key-value -1 "Key" "Value")))))

(ert-deftest kubernetes-ast-test--key-value--validates-key-as-string ()
  (with-temp-buffer
    (should-assert (kubernetes-ast-eval '(key-value 10 nil "Value")))))

(ert-deftest kubernetes-ast-test--key-value--validates-value-as-string ()
  (with-temp-buffer
    (should-assert (kubernetes-ast-eval '(key-value 10 "Key" 1)))))

(ert-deftest kubernetes-ast-test--key-value--ensures-values-are-inserted-on-new-lines ()
  (let ((ast '("foo" (key-value 10 "Key" "Value"))))
    (with-temp-buffer
      (save-excursion (kubernetes-ast-eval ast))
      (should (equal (format "foo\n%-10s%s\n" "Key:" "Value") (substring-no-properties (buffer-string)))))))


;; nav-prop

(ert-deftest kubernetes-ast-test--nav-prop ()
  (let ((ast '(nav-prop (:test "nav")
                        (line "Test"))))
    (with-temp-buffer
      (save-excursion (kubernetes-ast-eval ast))
      (should (equal "Test\n" (buffer-string)))
      (should (equal (list 'kubernetes-nav '(:test "nav"))
                     (text-properties-at (point-min) (buffer-string)))))))

(ert-deftest kubernetes-ast-test--nav-prop-finishes-at-end-of-line ()
  (let ((ast '(nav-prop (:test "nav")
                        (line "Test"))))
    (with-temp-buffer
      (save-excursion (kubernetes-ast-eval ast))
      (let ((end (1- (line-end-position))))
        (should (equal (list 'kubernetes-nav '(:test "nav"))
                       (text-properties-at end (buffer-string))))))))


;; copy-prop

(ert-deftest kubernetes-ast-test--copy-prop ()
  (let ((ast '(copy-prop "foo" (line "Test"))))
    (with-temp-buffer
      (save-excursion (kubernetes-ast-eval ast))
      (should (equal "Test\n" (buffer-string)))
      (should (equal (list 'kubernetes-copy "foo")
                     (text-properties-at (point-min) (buffer-string)))))))

(ert-deftest kubernetes-ast-test--copy-prop-finishes-at-end-of-line ()
  (let ((ast '(copy-prop "foo" (line "Test"))))
    (with-temp-buffer
      (save-excursion (kubernetes-ast-eval ast))
      (let ((end (1- (line-end-position))))
        (should (equal (list 'kubernetes-copy "foo")
                       (text-properties-at end (buffer-string))))))))

(ert-deftest kubernetes-ast-test--copy-prop-error-if-copy-value-not-a-string ()
  (let ((ast '(copy-prop 1 (line "Test"))))
    (with-temp-buffer
      (should-assert (kubernetes-ast-eval ast)))))


;; mark-for-delete

(ert-deftest kubernetes-ast-test--mark-for-delete-no-indentation ()
  (let ((ast '(mark-for-delete (line "Test"))))
    (with-temp-buffer
      (save-excursion (kubernetes-ast-eval ast))
      (should (equal "D Test\n" (buffer-string)))
      (should (equal '(face kubernetes-delete-mark)
                     (text-properties-at 0 (buffer-string))))
      (should (not (text-properties-at 1 (buffer-string)))))))

(ert-deftest kubernetes-ast-test--mark-for-delete-with-indentation ()
  (let ((ast '(mark-for-delete (line "  Test"))))
    (with-temp-buffer
      (save-excursion (kubernetes-ast-eval ast))
      (should (equal "D Test\n" (buffer-string)))
      (should (equal '(face kubernetes-delete-mark)
                     (text-properties-at 0 (buffer-string))))
      (should (not (text-properties-at 1 (buffer-string)))))))

(ert-deftest kubernetes-ast-test--mark-for-delete-multiple-lines ()
  (let ((ast '((mark-for-delete (line "foo")
                                (line "bar")
                                (line "baz"))
               (line "frotz"))))
    (with-temp-buffer
      (save-excursion (kubernetes-ast-eval ast))
      (should (equal "D foo\nD bar\nD baz\nfrotz\n" (buffer-string))))))


;; Test rendering function's point restoration.

(ert-deftest kubernetes-ast-test--render--no-err-if-empty-buffer ()
  (with-temp-buffer
    (kubernetes-ast-render (current-buffer) nil)
    (should (string-empty-p (buffer-string)))))

(ert-deftest kubernetes-ast-test--render--restores-point--1 ()
  (let ((ast nil))
    (with-temp-buffer
      (kubernetes-ast-render (current-buffer) ast))))

(ert-deftest kubernetes-ast-test--render--restores-point--2 ()
  (let ((expected-column) (expected-line)
        (ast '(line "foo")))
    (with-temp-buffer

      (kubernetes-ast-render (current-buffer) ast)
      (goto-char (point-min))
      (search-forward "o")

      (setq expected-column (current-column))
      (setq expected-line (line-number-at-pos))

      (kubernetes-ast-render (current-buffer) ast)

      (should (equal (current-column) expected-column))
      (should (equal (line-number-at-pos) expected-line)))))

(ert-deftest kubernetes-ast-test--render--restores-point--3 ()
  (let ((expected-column) (expected-line)
        (ast '((section (line1) (line "foo"))
               (section (line2) (line "bar"))
               (section (line3) (line "baz")))))
    (with-temp-buffer

      (kubernetes-ast-render (current-buffer) ast)
      (goto-char (point-min))
      (search-forward "ba")

      (setq expected-column (current-column))
      (setq expected-line (line-number-at-pos))

      (kubernetes-ast-render (current-buffer) ast)

      (should (equal (current-column) expected-column))
      (should (equal (line-number-at-pos) expected-line)))))

(defun kubernetes-ast-test--line-has-highlight-p ()
  (let ((overlay-faces-at-pt
         (--map (plist-get (overlay-properties it) 'face)
                (overlays-at (point)))))
    (memq 'magit-section-highlight overlay-faces-at-pt)))

(ert-deftest kubernetes-ast-test--render--updates-highlight ()
  (let ((ast '((section (line1) (line "foo"))
               (section (line2) (line "bar"))
               (section (line3) (line "baz")))))
    (with-temp-buffer
      (kubernetes-ast-render (current-buffer) ast)
      (should (kubernetes-ast-test--line-has-highlight-p))

      (goto-char (point-min))
      (search-forward "ba")
      (kubernetes-ast-render (current-buffer) ast)

      (should (kubernetes-ast-test--line-has-highlight-p))
      (should-not (save-excursion
                    (goto-char (point-min))
                    (kubernetes-ast-test--line-has-highlight-p))))))

(provide 'kubernetes-ast-test)

;;; kubernetes-ast-test.el ends here
