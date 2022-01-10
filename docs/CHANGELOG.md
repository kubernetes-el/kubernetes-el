---
title: Changelog
hide:
  - navigation
---

All notable changes to this project will be documented here.

The format is based on [Keep a Changelog][],
and this project adheres to [semantic
versioning][semver].

[Keep a Changelog]: https://keepachangelog.com/en/1.0.0/
[semver]: https://semver.org/spec/v2.0.0.html

## Upcoming

### Changed

-   New overview for persistent volume claims.
    ([#223](https://github.com/kubernetes-el/kubernetes-el/pull/223))
-   Added a new interactive function, `kubernetes-contexts-rename`, for
    renaming contexts.
    ([#231](https://github.com/kubernetes-el/kubernetes-el/pull/231))
-   Added a new transient prefix, `kubernetes-context`, for acting on
    kubectl contexts, e.g. switching, renaming, etc.
    ([#231](https://github.com/kubernetes-el/kubernetes-el/pull/231))
-   Quitting via \`Q\` from the overview or any `kubernetes-mode` buffer
    now terminates all in-flight background processes.
    ([#244](https://github.com/kubernetes-el/kubernetes-el/pull/244))
-   The overview for configmaps now includes a new section to show a
    (truncated) view of the data.
    ([#245](https://github.com/kubernetes-el/kubernetes-el/pull/245))
-   Added mapping for E to enable entity editing in evil mode.
    ([#246](https://github.com/kubernetes-el/kubernetes-el/pull/246))
-   `kubernetes-overview` now returns an error if `kubectl` or
    `kubernetes-kubectl-executable` is not found on PATH.

### Refinements

-   We've taken a big step towards [support for custom
    resources](https://github.com/kubernetes-el/kubernetes-el/issues/69),
    overhauling the process-tracking module – how `kubernetes-el` keeps
    track of the various `kubectl` processes that it spins up – to be
    resource agnostic
    ([#234](https://github.com/kubernetes-el/kubernetes-el/issues/234)).
    This removes another section of the codebase that historically has
    had to be updated for every new resource that `kubernetes-el` wants
    to "support," allowing it to accommodate any and all resources. See:
    [#237](https://github.com/kubernetes-el/kubernetes-el/pull/237);
    [#238](https://github.com/kubernetes-el/kubernetes-el/pull/238).

## 0.17.0

### Changed

-   Explicitly disable the `Exec into container using vterm` suffix of
    the `kubernetes-exec` transient if `vterm` is not installed
    ([#209](https://github.com/kubernetes-el/kubernetes-el/pull/209))

### Fixed

-   Some of the migrated transients from 0.16.0 were incomplete; we
    catch some (hopefully all of) the stragglers in this release. Thanks
    @noorul for the follow-through here.

-   Fixed a bug in Ingress display.
    ([#214](https://github.com/kubernetes-el/kubernetes-el/pull/214))

## 0.16.0

### Changed

-   Ability to find files in pods via `tramp`
    ([#167](https://github.com/kubernetes-el/kubernetes-el/pull/167))
-   Ability to exec into pods via
    [vterm](https://github.com/akermu/emacs-libvterm)
    ([#169](https://github.com/kubernetes-el/kubernetes-el/pull/169))
-   Ability to edit resources
    ([#186](https://github.com/kubernetes-el/kubernetes-el/pull/186))
-   Migrate several popups from the defunct \`magit-popup\` to
    \`transient\`
    ([#190](https://github.com/kubernetes-el/kubernetes-el/pull/190),
    [#193](https://github.com/kubernetes-el/kubernetes-el/pull/193),
    [#198](https://github.com/kubernetes-el/kubernetes-el/pull/198),
    etc.)
