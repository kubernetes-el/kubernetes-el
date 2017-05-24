# Build

```sh
make build
```
# Test

```sh
make test
```

# Run

```sh
./emacs-k8s -namespace my-namespace -interval 10
```

```sh
./emacs-k8s -h
Usage of ./emacs-k8s:
  -interval int
        Refresh interveal in seconds, defaults to 10 secounds (default 10)
  -namespace string
        Namespace that you want to use, empty "" for All namespaces
```
