;;; test-resources.el --- Tests for resource discovery -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load-file "./tests/undercover-init.el")

(require 'kubernetes-resources)

(describe "kubernetes--get-all-groups"
  (it "gets all the group names from the argument group list"
    (let ((input (json-read-from-string "{
  \"kind\": \"APIGroupList\",
  \"apiVersion\": \"v1\",
  \"groups\": [
    {
      \"name\": \"apiregistration.k8s.io\",
      \"versions\": [
        {
          \"groupVersion\": \"apiregistration.k8s.io/v1\",
          \"version\": \"v1\"
        },
        {
          \"groupVersion\": \"apiregistration.k8s.io/v1beta1\",
          \"version\": \"v1beta1\"
        }
      ],
      \"preferredVersion\": {
        \"groupVersion\": \"apiregistration.k8s.io/v1\",
        \"version\": \"v1\"
      }
    },
    {
      \"name\": \"apps\",
      \"versions\": [
        {
          \"groupVersion\": \"apps/v1\",
          \"version\": \"v1\"
        }
      ],
      \"preferredVersion\": {
        \"groupVersion\": \"apps/v1\",
        \"version\": \"v1\"
      }
    },
    {
      \"name\": \"events.k8s.io\",
      \"versions\": [
        {
          \"groupVersion\": \"events.k8s.io/v1\",
          \"version\": \"v1\"
        }
      ],
      \"preferredVersion\": {
        \"groupVersion\": \"events.k8s.io/v1\",
        \"version\": \"v1\"
      }
    }
  ]
}")))
      (expect (kubernetes--get-all-groups input)
              :to-equal
              '("apiregistration.k8s.io"
                "apps"
                "events.k8s.io")))))

;;; test-resources.el ends here
