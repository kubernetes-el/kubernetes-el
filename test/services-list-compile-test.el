;;; services-list-compile-test.el --- Test rendering of the services list  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-and-compile
  (require 'f)

  (defvar project-root
    (locate-dominating-file default-directory ".git"))

  (defvar this-directory
    (f-join project-root "test")))

(require 'kubernetes (f-join project-root "kubernetes.el"))

(defconst sample-get-services-response
  (let* ((path (f-join this-directory "get-services-response.json"))
         (sample-response (f-read-text path)))
    (json-read-from-string sample-response)))

(defun draw-services-section (state)
  (kubernetes--eval-ast (kubernetes--render-services-section state)))


;; Shows "Fetching..." when state isn't initialized yet.

(defconst drawing-services-section-loading-result
  (s-trim-left "

Services
  Name                                InternalIP      ExternalIP    Age
  Fetching...

"))

(ert-deftest drawing-services-section--empty-state ()
  (with-temp-buffer
    (save-excursion (magit-insert-section (root)
                      (draw-services-section nil)))
    (should (equal drawing-services-section-loading-result
                   (substring-no-properties (buffer-string))))
    (forward-line 1)
    (forward-to-indentation)
    (should (equal 'kubernetes-progress-indicator (get-text-property (point) 'face)))))


;; Shows "None" when there are no services.

(defconst drawing-services-section-empty-result
  (s-trim-left "

Services (0)
  None.

"))

(ert-deftest drawing-services-section--no-services ()
  (let ((empty-state `((services . ((items . ,(vector)))))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-services-section empty-state)))
      (should (equal drawing-services-section-empty-result
                     (substring-no-properties (buffer-string))))
      (search-forward "None")
      (should (equal 'magit-dimmed (get-text-property (point) 'face))))))


;; Shows service lines when there are services.

(defconst drawing-services-section-sample-result
  (s-trim-left "

Services (2)
  Name                                InternalIP      ExternalIP    Age
  example-svc-1                      192.168.0.0     192.168.0.0     4d
    Namespace:     example
    Created:       2017-03-29T21:42:56Z
    Internal IP:   192.168.0.0
    External IPs:  192.168.0.0
    Ports:         80/TCP

  example-svc-2                      192.168.0.0                    19d
    Namespace:     example
    Created:       2017-03-14T21:42:56Z
    Internal IP:   192.168.0.0
    Ports:         80/TCP


"))

(ert-deftest drawing-services-section--sample-response ()
  (let ((state `((services . ,sample-get-services-response)
                 (current-time . ,(date-to-time "2017-04-03 00:00Z")))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-services-section state)))
      (should (equal drawing-services-section-sample-result
                     (substring-no-properties (buffer-string)))))))

(ert-deftest drawing-services-section--sample-response-text-properties ()
  (let ((state `((services . ,sample-get-services-response))))
    (with-temp-buffer
      (save-excursion (magit-insert-section (root)
                        (draw-services-section state)))
      ;; Skip past header.
      (forward-line 2)

      (dolist (key '("Namespace"
                     "Created"))
        (save-excursion
          (search-forward key)
          (should (equal 'magit-header-line (get-text-property (point) 'face))))))))


;;; services-list-compile-test.el ends here
