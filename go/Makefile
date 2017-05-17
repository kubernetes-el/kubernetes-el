VENDOR = vendor

GO ?= go
GOOS ?= $(shell uname | tr [:upper:] [:lower:])
GOARCH ?= amd64

SRCS = $(wildcard *.go)
TARGET = emacs-k8s


.PHONY: all build clean test test-short

all: build

build: $(TARGET)

clean:
	rm -f $(TARGET)

test:
	$(GO) test -v -cover

test-short:
	$(GO) test -v -short

$(VENDOR):
	dep ensure

$(TARGET): $(SRCS) $(VENDOR)
	GOOS=${GOOS} GOARCH=${GOARCH} CGO_ENABLED=0 $(GO) build -o $(TARGET) -a .
