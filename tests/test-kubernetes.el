;;; test-kubernetes.el --- Tests for general constructs. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load-file "./tests/undercover-init.el")

(require 'kubernetes)

(describe "kubernetes--with-proxy"
  (before-each
    (spy-on 'get-proxy-process :and-return-value :sentinel-proxy-process))

  (it "can access the proxy process via `it'"
    (kubernetes--with-proxy
        (expect it :to-equal :sentinel-proxy-process))))

;;; test-kubernetes.el ends here
