# Comparisons with Similar Packages/Tools

This page compares `kubernetes-el` with some of its peer packages.

## [Kele]

[Kele] is an early-stage Kubernetes cluster management package for Emacs.

[Kele] aims to provide a cluster management interface that is more piecemeal and
targeted than `kubernetes-el`. Kele optimizes for minimal context-switching,
providing quick and targeted keybindings and nested command trees that enable
you to get the answers that you need -- or carry out the tasks that you need to
carry out -- and return to your other work as quickly as possible.

One advantage that [Kele] has over `kubernetes-el` is its **day-one support for
custom resources** and the full Kubernetes core resource API -- Pods,
Deployments, DaemonSets, etc. `kubernetes-el`'s lack of support for both is a
long-standing drawback of the package (see [`kubernetes-el/kubernetes-el#69`]).

On the other hand, `kubernetes-el` has far more of a "GUI" than Kele. It centers
heavily around an "overview"-based interface and is, as a result, more suited to
in-depth, intentful, focused exploration of one's cluster(s).

## [kubel]

[kubel] is a similar "UI-centric" cluster management package to
[kubernetes-el]. Its advantage over [kubernetes-el] is its accommodation of
users with limited privilege/permissions within the clusters in question.

[kubel]: https://github.com/abrochard/kubel
[Kele]: https://jonathanj.in/kele.el
[`kubernetes-el/kubernetes-el#69`]: https://github.com/kubernetes-el/kubernetes-el/issues/69
[`kubernetes-el/kubernetes-el#306`]: https://github.com/kubernetes-el/kubernetes-el/issues/306
