# kubernetes-el

[![Build Status](https://github.com/kubernetes-el/kubernetes-el/actions/workflows/ci.yaml/badge.svg?branch=master)](https://github.com/kubernetes-el/kubernetes-el/actions/workflows/ci.yaml)
[![Coverage Status](https://coveralls.io/repos/github/kubernetes-el/kubernetes-el/badge.svg?branch=master)](https://coveralls.io/github/kubernetes-el/kubernetes-el?branch=master)
[![MELPA Stable](https://stable.melpa.org/packages/kubernetes-badge.svg)](https://stable.melpa.org/#/kubernetes)
[![MELPA](https://melpa.org/packages/kubernetes-badge.svg)](https://melpa.org/#/kubernetes)
[![License GPL 3](https://img.shields.io/github/license/kubernetes-el/kubernetes-el.svg)][COPYING]

A magit-style interface to the Kubernetes command-line client.

![Screenshot of Kubernetes Emacs client](assets/screenshot.png)

This project is in early stages, but the following are implemented:

- Live-updating lists of kubernetes resources
- Viewing and deleting pods, configmaps and secrets
- Switching contexts and namespaces.
- Showing logs and exec'ing into containers
- Describing pods

## Usage

Run `M-x kubernetes-overview` to get started.

## Installation

This package is available on the [MELPA][] package repository. See the
instructions there for how to configure Emacs to pull packages from MELPA.

Once you've set that up, use your preferred method of configuring and installing
packages. If you use [use-package][], the forms below will get you started.

```elisp
(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

;; If you want to pull in the Evil compatibility package.
(use-package kubernetes-evil
  :ensure t
  :after kubernetes)
```

Otherwise, you can install the packages with `M-x package-install`.

Disabling automatic refresh helped many to solve [issue #100][]. For
example one can use the following configuration:

```elisp
(kubernetes-poll-frequency 3600)
(kubernetes-redraw-frequency 3600)
```

## Manual Installation

Requires Emacs 25 and [Cask][].

```sh
git clone git@github.com:kubernetes-el/kubernetes-el.git
cd kubernetes-el
make && make install
```

## Contributing

Yes please! 😻 See [contributing.org][]


[Cask]: https://github.com/cask/cask
[COPYING]: ./COPYING
[Evil]: https://github.com/emacs-evil/evil
[MELPA]: http://melpa.milkbox.net/#/getting-started
[contributing.org]: ./contributing.org
[use-package]: https://github.com/jwiegley/use-package
[issue #100]: https://github.com/kubernetes-el/kubernetes-el/issues/100
