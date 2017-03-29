# kubernetes-el

A magit-style interface to the Kubernetes command-line client.

This project is in early stages, but the following are implemented:

- Live-updating list of pods
- Showing logs for the pods
- Exec'ing into a pod's docker container
- Deleting pods
- Inspecting pods.

## Usage

Run `M-x kubernetes-display-pods` to get started.

## Installation

Clone this repo and run `M-x package-install-file kubernetes.el`. If you use
[Evil][], you might like to install `kubernetes-evil.el` too.

Once you've set that up, use your preferred method of configuring packages. If
you use [use-package][], the form below will get you started.

```elisp
(use-package kubernetes
  :commands (kubernetes-display-pods)
  :config
  (use-package kubernetes-evil :after evil))
```

[Evil]: https://github.com/emacs-evil/evil
[use-package]: https://github.com/jwiegley/use-package
