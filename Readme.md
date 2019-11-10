# kubernetes-el

[![Build Status](https://travis-ci.org/chrisbarrett/kubernetes-el.svg?branch=master)](https://travis-ci.org/chrisbarrett/kubernetes-el)
[![Coverage Status](https://coveralls.io/repos/github/chrisbarrett/kubernetes-el/badge.svg?branch=master)](https://coveralls.io/github/chrisbarrett/kubernetes-el?branch=master)
[![MELPA Stable](http://stable.melpa.org/packages/kubernetes-badge.svg)](http://stable.melpa.org/#/kubernetes)
[![MELPA](http://melpa.milkbox.net/packages/kubernetes-badge.svg)](http://melpa.milkbox.net/#/kubernetes)
[![License GPL 3](https://img.shields.io/github/license/chrisbarrett/kubernetes-el.svg)][COPYING]

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

## Manual Installation

Requires Emacs 25 and [Cask][].

```sh
git clone git@github.com:chrisbarrett/kubernetes-el.git
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
