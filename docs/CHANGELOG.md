# Changelog

All notable changes to this project will be documented here.

The format is based on [Keep a Changelog][],
and this project adheres to [semantic
versioning][semver].

[Keep a Changelog]: https://keepachangelog.com/en/1.0.0/
[semver]: https://semver.org/spec/v2.0.0.html

## Upcoming

## 0.19.0

### Added

- Add Kubernetes events overview.
  ([#346](https://github.com/kubernetes-el/kubernetes-el/pull/346))
- Add dedicated package for exec functionality.
  ([#349](https://github.com/kubernetes-el/kubernetes-el/pull/349))
- Add buffer switching functionality for Kubernetes logs.
  ([#351](https://github.com/kubernetes-el/kubernetes-el/pull/351))
- Add support for init containers display in pods overview.
  ([#363](https://github.com/kubernetes-el/kubernetes-el/pull/363))
- Add support for NetworkPolicies.
  ([#314](https://github.com/kubernetes-el/kubernetes-el/pull/314))
- Add basic CronJobs rendering support.
  ([#335](https://github.com/kubernetes-el/kubernetes-el/pull/335))
- Add generic resource description functionality.
  ([#342](https://github.com/kubernetes-el/kubernetes-el/pull/342))
- Add support for additional resource types in logs command (services, etc.).
  ([#340](https://github.com/kubernetes-el/kubernetes-el/pull/340),
  [#344](https://github.com/kubernetes-el/kubernetes-el/pull/344))
- Add option to get logs from all pods with prefixed output.
  ([#343](https://github.com/kubernetes-el/kubernetes-el/pull/343))
- Add enhanced resource selection for exec and logs, prioritizing manually selected resources.
  ([#364](https://github.com/kubernetes-el/kubernetes-el/pull/364),
  [#366](https://github.com/kubernetes-el/kubernetes-el/pull/366),
  [#367](https://github.com/kubernetes-el/kubernetes-el/pull/367))
- Enable discovery of API groups from cluster.
  ([#302](https://github.com/kubernetes-el/kubernetes-el/pull/302))
- Add `dash` package dependency.
  ([#320](https://github.com/kubernetes-el/kubernetes-el/pull/320))

### Changed

- Rename `kubernetes-exec-list-buffers` to `kubernetes-exec-switch-buffers`.
  ([#352](https://github.com/kubernetes-el/kubernetes-el/pull/352))
- Standardize buffer naming and improve buffer selection.
  ([#350](https://github.com/kubernetes-el/kubernetes-el/pull/350))
- Simplify pod display in overview and improve pod state handling.
  ([#361](https://github.com/kubernetes-el/kubernetes-el/pull/361))
- Use `ownerReferences` attribute to build resource relationships.
  ([#338](https://github.com/kubernetes-el/kubernetes-el/pull/338))
- Enhance logs management with buffer naming and refresh.
  ([#341](https://github.com/kubernetes-el/kubernetes-el/pull/341))
- Simplify resource editing with generic utility function.
  ([#339](https://github.com/kubernetes-el/kubernetes-el/pull/339))
- Remove strict dependency pin on Emacs 25.1.
  ([#296](https://github.com/kubernetes-el/kubernetes-el/pull/296))
- Remove `define-getter` and `define-accessors` macros.
  ([#305](https://github.com/kubernetes-el/kubernetes-el/pull/305))
- Remove compatibility alias `kubernetes-utils-read-pod-name`.
  ([#301](https://github.com/kubernetes-el/kubernetes-el/pull/301))

### Fixed

- Fix consistent resource path format across kubectl interactions.
  ([#355](https://github.com/kubernetes-el/kubernetes-el/pull/355))
- Fix interactive functionality in exec and logs.
  ([#368](https://github.com/kubernetes-el/kubernetes-el/pull/368))
- Fix pod status handling and pod-deployment relation tracking.
  ([#345](https://github.com/kubernetes-el/kubernetes-el/pull/345))
- Fix `"v"` key map in the Kubernetes dispatch menu.
  ([#348](https://github.com/kubernetes-el/kubernetes-el/pull/348))
- Fix log message on switching namespace to properly reflect the selected namespace.
  ([#297](https://github.com/kubernetes-el/kubernetes-el/pull/297))
- Fix faulty macro definition that prevented the overview buffer from successfully refreshing on Emacs 29.x.
  ([#303](https://github.com/kubernetes-el/kubernetes-el/pull/303))

## 0.18.0

### Changed

- New flag `--timestamps` support added for logs command.
  ([#270](https://github.com/kubernetes-el/kubernetes-el/pull/270))
- New overview for persistent volume claims.
  ([#223](https://github.com/kubernetes-el/kubernetes-el/pull/223))
- Added a new interactive function, `kubernetes-contexts-rename`, for
  renaming contexts.
  ([#231](https://github.com/kubernetes-el/kubernetes-el/pull/231))
- Added a new transient prefix, `kubernetes-context`, for acting on
  kubectl contexts, e.g. switching, renaming, etc.
  ([#231](https://github.com/kubernetes-el/kubernetes-el/pull/231))
- Quitting via \`Q\` from the overview or any `kubernetes-mode` buffer
  now terminates all in-flight background processes.
  ([#244](https://github.com/kubernetes-el/kubernetes-el/pull/244))
- The overview for configmaps now includes a new section to show a
  (truncated) view of the data.
  ([#245](https://github.com/kubernetes-el/kubernetes-el/pull/245))
- Added mapping for E to enable entity editing in evil mode.
  ([#246](https://github.com/kubernetes-el/kubernetes-el/pull/246)).
- Added ability to enable and disable [`kubectl` proxies][kubectl proxy] via
  `P P`, with status display in the overview. ([#252](https://github.com/kubernetes-el/kubernetes-el/pull/252))
- `kubernetes-overview` now returns an error if `kubectl` or
  `kubernetes-kubectl-executable` is not found on PATH.

[kubectl proxy]: https://kubernetes.io/docs/tasks/extend-kubernetes/http-proxy-access-api/

### Refinements

- We've taken a big step towards [support for custom
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

### Fixed

- Kubernetes tramp was not respecting set namespace as there are limitations to what we can pass to `tramp-login-args`
  ([#264](https://github.com/kubernetes-el/kubernetes-el/issues/264)).  The fix adds one more step to update the
  `kubectl` configuration.
- Fixed an implicit dependency cycle between `kubernetes-core.el` and `kubernetes-utils.el`. ([#278])
- Fixed an issue where loading the package reports that `kubernetes-tramp-find-file` is not defined.

## 0.17.0

### Changed

- Explicitly disable the `Exec into container using vterm` suffix of
  the `kubernetes-exec` transient if `vterm` is not installed
  ([#209](https://github.com/kubernetes-el/kubernetes-el/pull/209))

### Fixed

- Some of the migrated transients from 0.16.0 were incomplete; we
  catch some (hopefully all of) the stragglers in this release. Thanks
  @noorul for the follow-through here.

- Fixed a bug in Ingress display.
  ([#214](https://github.com/kubernetes-el/kubernetes-el/pull/214))

## 0.16.0

### Changed

- Ability to find files in pods via `tramp`
  ([#167](https://github.com/kubernetes-el/kubernetes-el/pull/167))
- Ability to exec into pods via
  [vterm](https://github.com/akermu/emacs-libvterm)
  ([#169](https://github.com/kubernetes-el/kubernetes-el/pull/169))
- Ability to edit resources
  ([#186](https://github.com/kubernetes-el/kubernetes-el/pull/186))
- Migrate several popups from the defunct \`magit-popup\` to
  \`transient\`
  ([#190](https://github.com/kubernetes-el/kubernetes-el/pull/190),
  [#193](https://github.com/kubernetes-el/kubernetes-el/pull/193),
  [#198](https://github.com/kubernetes-el/kubernetes-el/pull/198),
  etc.)

[#278]: https://github.com/kubernetes-el/kubernetes-el/pull/278
