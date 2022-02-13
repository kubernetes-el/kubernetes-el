;;; test-tramp.el --- Tests for Kubernetes TRAMP  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load-file "./tests/undercover-init.el")

(require 'kubernetes-el-tramp)

(describe "Form tramp file path using pod-name and container name"
          (it "should return path with pod name and without container name"
              (expect (get--tramp-file-path "pod0") :to-equal "/kubernetes:pod0:"))
          (it "should return path with both pod name and container name"
              (expect (get--tramp-file-path "pod0" "container0") :to-equal "/kubernetes:container0@pod0:")))
