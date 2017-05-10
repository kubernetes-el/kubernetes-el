ARTIFACT = emacs-k8s
OS = $(shell uname | tr [:upper:] [:lower:])

all: build

build: GOOS ?= ${OS}
build: GOARCH ?= amd64
build: clean
		@echo "building..."; \
		GOOS=${GOOS} GOARCH=${GOARCH} CGO_ENABLED=0 go build -o ${ARTIFACT} -a .

clean:
		rm -f ${ARTIFACT}



test:
		go test -v -cover

test-short:
		go test -v -short
