# kubernetes-el

[![Build Status](https://github.com/kubernetes-el/kubernetes-el/actions/workflows/ci.yaml/badge.svg?branch=master)](https://github.com/kubernetes-el/kubernetes-el/actions/workflows/ci.yaml)
[![codecov](https://codecov.io/gh/kubernetes-el/kubernetes-el/branch/master/graph/badge.svg?token=DVOij2iMVs)](https://codecov.io/gh/kubernetes-el/kubernetes-el)
[![MELPA Stable](https://stable.melpa.org/packages/kubernetes-badge.svg)](https://stable.melpa.org/#/kubernetes)
[![MELPA](https://melpa.org/packages/kubernetes-badge.svg)](https://melpa.org/#/kubernetes)
[![License GPL 3](https://img.shields.io/github/license/kubernetes-el/kubernetes-el.svg)][COPYING]

Manage your Kubernetes clusters with Emacs.

![Screenshot of Kubernetes Emacs client](./docs/assets/screenshot.png)

See [docs/index.md](./docs/index.md) for details.

## Known Issues

Disabling automatic refresh helped many to solve [issue #100][]. For
example one can use the following configuration:

```elisp
(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))
```


[Cask]: https://github.com/cask/cask
[COPYING]: ./COPYING
[Evil]: https://github.com/emacs-evil/evil
[contributing.org]: ./contributing.org
[issue #100]: https://github.com/kubernetes-el/kubernetes-el/issues/100
