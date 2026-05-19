;;; kubernetes-core-test.el --- Tests for kubernetes-core.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'kubernetes-core)
(require 'kubernetes-ast)
(require 'magit-section)


;; kubernetes--hide-sections-marked-hidden walks the section tree and creates
;; the invisibility overlay for every section whose `hidden' slot is non-nil.
;; magit-section's HIDE argument only sets the slot; without this helper the
;; body text below the heading is inserted into the buffer but never
;; visually hidden.

(defun kubernetes-core-test--invisible-overlays-in (beg end)
  (seq-filter (lambda (overlay) (overlay-get overlay 'invisible))
              (overlays-in beg end)))

(ert-deftest kubernetes-core-test--hide-sections-marked-hidden__when-section-marked-hidden__should-create-invisible-overlay-over-body ()
  (with-temp-buffer
    (kubernetes-ast-eval
     '(section (root nil)
               (section (child t)
                        (heading "header")
                        (line "body line 1")
                        (line "body line 2"))))
    (kubernetes--hide-sections-marked-hidden)
    (let* ((child (car (oref magit-root-section children)))
           (overlays (kubernetes-core-test--invisible-overlays-in
                      (oref child content)
                      (oref child end))))
      (should (equal 1 (length overlays)))
      (should (= (oref child content) (overlay-start (car overlays))))
      (should (= (oref child end) (overlay-end (car overlays)))))))

(ert-deftest kubernetes-core-test--hide-sections-marked-hidden__when-section-not-marked-hidden__should-not-create-overlay ()
  (with-temp-buffer
    (kubernetes-ast-eval
     '(section (root nil)
               (section (child nil)
                        (heading "header")
                        (line "body line"))))
    (kubernetes--hide-sections-marked-hidden)
    (let* ((child (car (oref magit-root-section children)))
           (overlays (kubernetes-core-test--invisible-overlays-in
                      (oref child content)
                      (oref child end))))
      (should (equal 0 (length overlays))))))

(ert-deftest kubernetes-core-test--hide-sections-marked-hidden__when-visibility-cache-says-show__should-not-hide ()
  (with-temp-buffer
    (let ((magit-section-visibility-cache
           '((((child . nil) (root . nil)) . show))))
      (kubernetes-ast-eval
       '(section (root nil)
                 (section (child t)
                          (heading "header")
                          (line "body line"))))
      (kubernetes--hide-sections-marked-hidden)
      (let* ((child (car (oref magit-root-section children)))
             (overlays (kubernetes-core-test--invisible-overlays-in
                        (oref child content)
                        (oref child end))))
        (should (equal 0 (length overlays)))))))

(ert-deftest kubernetes-core-test--hide-sections-marked-hidden__when-nested-sections-marked-hidden__should-hide-each-one ()
  (with-temp-buffer
    (kubernetes-ast-eval
     '(section (root nil)
               (section (outer t)
                        (heading "outer header")
                        (section (inner t)
                                 (heading "inner header")
                                 (line "inner body")))))
    (kubernetes--hide-sections-marked-hidden)
    (let* ((outer (car (oref magit-root-section children)))
           (inner (car (oref outer children)))
           (outer-overlays (kubernetes-core-test--invisible-overlays-in
                            (oref outer content)
                            (oref outer end)))
           (inner-overlays (kubernetes-core-test--invisible-overlays-in
                            (oref inner content)
                            (oref inner end))))
      (should (equal 1 (length outer-overlays)))
      (should (equal 1 (length inner-overlays))))))

(ert-deftest kubernetes-core-test--hide-sections-marked-hidden__when-magit-root-section-nil__should-not-error ()
  (with-temp-buffer
    (let ((magit-root-section nil))
      (should-not (kubernetes--hide-sections-marked-hidden)))))

(ert-deftest kubernetes-core-test--hide-sections-marked-hidden__when-section-has-no-content__should-skip ()
  (with-temp-buffer
    (kubernetes-ast-eval
     '(section (root nil)
               (section (headless t)
                        (line "no heading here"))))
    (let* ((headless (car (oref magit-root-section children))))
      (should-not (oref headless content))
      (should-not (kubernetes--hide-sections-marked-hidden))
      (should (equal 0 (length (kubernetes-core-test--invisible-overlays-in
                                (oref headless start)
                                (oref headless end))))))))

;;; kubernetes-core-test.el ends here
