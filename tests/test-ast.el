;;; test-ast.el --- Tests for AST parsing/formatting -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load-file "./tests/undercover-init.el")

(require 'buttercup)
(require 'dash)
(require 'kubernetes-ast)

(defmacro with-rendered-ast (ast &rest body)
  "Render AST to temporary buffer and run BODY therein."
  (declare (indent defun))
  `(with-temp-buffer
    (save-excursion (kubernetes-ast-eval ,ast))
    ,@body))

(buttercup-define-matcher :to-render-as (ast s-expect)
  (cl-destructuring-bind
      ((ast-expr . ast) (s-expect-expr . s-expect))
      (mapcar #'buttercup--expr-and-value (list ast s-expect))
    (with-temp-buffer
      (kubernetes-ast-eval ast)
      (buttercup--test-expectation
          (funcall 'equal (buffer-string) s-expect)
        :expect-match-phrase
        (format "Expected %s to render to \"%s\", but it rendered to \"%s\"" ast-expr s-expect (buffer-string))
        :expect-mismatch-phrase "Expected %s not to render to \"%s\", but it did."))))

(describe "AST rendering"
  (it "rejects invalid AST"
    (let ((ast '(foo "bar")))
      (expect (kubernetes-ast-eval ast) :to-throw 'error)))

  (describe "padding"
    (it "inserts newline for padding"
      (expect '(padding) :to-render-as "\n"))

    (it "errors if applied to arguments"
      (let ((ast '(padding foo)))
        (with-temp-buffer
          (expect (kubernetes-ast-eval ast) :to-throw 'error)))))

  (describe "lines"
    (it "inserts newlines for empty strings"
      (expect '(line "") :to-render-as "\n"))
    (it "inserts strings"
      (expect '(line "foo") :to-render-as "foo\n"))
    (it "handles sequences"
      (expect '((line "foo") (line "bar")) :to-render-as "foo\nbar\n")))

  (describe "lists"
    (it "handles string elements"
      (expect
       '(list (line "foo") (line "bar") (line "baz"))
       :to-render-as
       (string-trim-left "
- foo
- bar
- baz
")))
    (it "handles dashes in elements"
      (expect
       '(list (line "foo") (line "-bar") (line "--baz"))
       :to-render-as
       (string-trim-left "
- foo
- -bar
- --baz
")))
    (it "handles empty strings"
      (expect
       '(list (line ""))
       :to-render-as
       (string-trim-left "
- 
")))
    (it "handles multiple lines"
      (expect
       '(list ((line "foo") (line "bar"))
              ((line "foo") (line "  bar"))
              ("foo" "bar"))
       :to-render-as
       (string-trim-left "
- foo
  bar
- foo
    bar
- foobar")))

    (it "handles nesting"
      (expect
       '(list (line "foo")
              (line "bar")
              (list (line "foo")
                    (line "bar"))
              (line "baz"))
       :to-render-as
       (string-trim-left "
- foo
- bar
  - foo
  - bar
- baz
")))
    )

  (describe "propertization"
    (it "handles regions"
      (expect
       '(propertize (face error) (line "foo"))
       :to-render-as
       (propertize "foo\n" 'face 'error)))
    )

  (describe "headings"
    :var ((ast '(heading "hello")))
    (it "inserts successfully inside sections"
      (with-temp-buffer
        (save-excursion
          (magit-insert-section (test)
            (kubernetes-ast-eval ast)))
        (expect
         (substring-no-properties (buffer-string))
         :to-equal
         "hello\n")))
    (it "assigns proper face"
      (with-temp-buffer
        (save-excursion
          (magit-insert-section (test)
            (kubernetes-ast-eval ast)))
        (expect (face-at-point) :to-equal 'magit-section-heading)))
    (it "errors if inserting outside section"
      (expect (kubernetes-ast-eval ast) :to-throw 'error)))

  (describe "sections"
    :var ((ast '(section (test nil) (line "foo"))))
    (it "renders properly"
      (expect ast :to-render-as "foo\n"))

      (it "should be recognized as such"
        (with-rendered-ast ast
          (expect (magit-current-section) :to-be-truthy)))
      (it "should insert the current section type"
        (with-rendered-ast ast
          (expect (oref (magit-current-section) type) :to-equal 'test)))
      (it "should show the section"
        (with-rendered-ast ast
          (expect (oref (magit-current-section) hidden) :not :to-be-truthy))))

  (describe "indentation"))

;;; test-ast.el ends here
