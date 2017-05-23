package main

import (
	"fmt"
	"io/ioutil"
	"os"

	"github.com/ericchiang/k8s"
	"github.com/ghodss/yaml"
	"github.com/mitchellh/go-homedir"
)

type k8sClient interface {
	CoreV1() *k8s.CoreV1
}

func loadDefaultClient() (k8sClient, error) {
	home, err := homedir.Dir()
	if err != nil {
		panic(err)
	}

	// Change to HOME/.kube so that relative key files work in k8s config
	kubeDir, err := os.Open(home + "/.kube")
	if err != nil {
		panic(err)
	}
	err = kubeDir.Chdir()
	if err != nil {
		panic(err)
	}

	return loadClient(home + "/.kube/config")
}

func loadClient(kubeconfigPath string) (*k8s.Client, error) {
	data, err := ioutil.ReadFile(kubeconfigPath)
	if err != nil {
		return nil, fmt.Errorf("read kubeconfig: %v", err)
	}

	var config k8s.Config
	if err := yaml.Unmarshal(data, &config); err != nil {
		return nil, fmt.Errorf("unmarshal kubeconfig: %v", err)
	}
	return k8s.NewClient(&config)
}
