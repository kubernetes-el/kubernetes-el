# kubernetes-el

A magit-style interface to the Kubernetes command-line client.

![Screenshot of Kubernetes Emacs client](assets/screenshot.png)

This project is in early stages, but the following are implemented:

- Live-updating lists of pods and configmaps
- Showing logs for pods
- Exec'ing into a pod's docker container
- Getting configmaps
- Describing pods
- Deleting pods and configmaps
- Switching contexts and namespaces.

## Usage

Run `M-x kubernetes-display-pods` or `M-x kubernetes-display-configmaps` to get started.

## Installation

Requires Emacs 25 and [Cask][].

```sh
git clone git@github.com:chrisbarrett/kubernetes-el.git
cd kubernetes-el
make && make install
```

Once you've set that up, use your preferred method of configuring packages. If
you use [use-package][], the form below will get you started.

```elisp
(use-package kubernetes
  :commands (kubernetes-display-pods 
             kubernetes-display-configmaps)
  :config
  (use-package kubernetes-evil :after evil))
```

[Cask]: https://github.com/cask/cask
[Evil]: https://github.com/emacs-evil/evil
[use-package]: https://github.com/jwiegley/use-package
