# kubernetes-el

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
  :commands (kubernetes-overview
             kubernetes-display-pods
             kubernetes-display-configmaps
             kubernetes-display-secrets)

  :config
  (use-package kubernetes-evil :after evil))
```

## Contributing

Yes please! 😻 See [contributing.org][]


[contributing.org]: ./contributing.org
[Cask]: https://github.com/cask/cask
[Evil]: https://github.com/emacs-evil/evil
[use-package]: https://github.com/jwiegley/use-package
