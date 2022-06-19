---
hide:
  - navigation
---

# kubernetes-el

Manage your Kubernetes clusters with Emacs.

![Screenshot of Kubernetes Emacs client](./assets/screenshot.png)

## Feature Overview

With `kubernetes-el`, you can:

- View live-updated lists of Kubernetes resources;
- View manifests for core Kubernetes resources (Pods, Configmaps, Secrets,
  etc.);
- Edit various core Kubernetes resource kinds, e.g. Deployments and Configmaps;
- Fetch and follow logs;
- Exec into containers;
- Describe pods;
- Start and terminate a kubectl [proxy server][kubectl proxy];
- Much more.

[kubectl proxy]: https://kubernetes.io/docs/tasks/extend-kubernetes/http-proxy-access-api/

## Development Roadmap

The project is actively being developed.

For known work items, see our [Issues page][issues].

For discussions about higher-level direction of the project and development
processes, see our [Discussions page][discussions].

## Compatibility

### Key

| Definition                                   | Icon                       |
|----------------------------------------------|:--------------------------:|
| Fully supported                              | :octicons-check-circle-24: |
| Tested against, but not officially supported | :octicons-circle-24:       |
| Unsupported                                  | :octicons-x-circle-24:     |

### Emacs

| Version | Compatibility              |
|:--------|:---------------------------|
| 25.x    | :octicons-check-circle-24: |
| 26.x    | :octicons-check-circle-24: |
| 27.x    | :octicons-check-circle-24: |
| 28.x    | :octicons-check-circle-24: |
| 29.x    | :octicons-circle-24:       |

### Kubernetes Servers

!!! note

    More explicit guarantees around Kubernetes compatibility is in the
    works. See [discussion #236][] for details.

We have no guarantees around Kubernetes server compatibility currently. Please
report any issues to us that you encounter with specific versions.

### `kubectl`

!!! note

    More explicit guarantees around Kubernetes compatibility is in the
    works. See [discussion #236][] for details.

We have no guarantees around `kubectl` compatibility currently. Please report
any issues to us that you encounter with specific versions.

## Contributing

Yes please! 😻 See [Contributing](contributing.md) for details.

[COPYING]: ./COPYING
[Evil]: https://github.com/emacs-evil/evil
[MELPA]: http://melpa.milkbox.net/#/getting-started
[contributing.org]: ./contributing.org
[use-package]: https://github.com/jwiegley/use-package
[issue #100]: https://github.com/kubernetes-el/kubernetes-el/issues/100

[issues]: github.com/kubernetes-el/kubernetes-el/issues
[discussions]: https://github.com/kubernetes-el/kubernetes-el/discussions
[discussion #236]: https://github.com/kubernetes-el/kubernetes-el/discussions/236
