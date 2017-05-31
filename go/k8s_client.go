package main

import (
	"context"
	"fmt"
	"io/ioutil"
	"os"

	"github.com/ericchiang/k8s"
	api "github.com/ericchiang/k8s/api/v1"
	apps "github.com/ericchiang/k8s/apis/apps/v1beta1"
	"github.com/ghodss/yaml"
	"github.com/mitchellh/go-homedir"
)

type k8sClient interface {
	ListPods(ctx context.Context, namespace string, options ...k8s.Option) (*api.PodList, error)
}

type appsClient interface {
	ListDeployments(ctx context.Context, namespace string, options ...k8s.Option) (*apps.DeploymentList, error)
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

func loadClient(kubeconfigPath string) (k8sClient, error) {
	data, err := ioutil.ReadFile(kubeconfigPath)
	if err != nil {
		return nil, fmt.Errorf("read kubeconfig: %v", err)
	}

	var config k8s.Config
	if err := yaml.Unmarshal(data, &config); err != nil {
		return nil, fmt.Errorf("unmarshal kubeconfig: %v", err)
	}
	c, err := k8s.NewClient(&config)
	if err != nil {
		return nil, err
	}
	return c.CoreV1(), nil
}

func loadDefaultAppsClient() (appsClient, error) {
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

	return loadAppsClient(home + "/.kube/config")
}

func loadAppsClient(kubeconfigPath string) (appsClient, error) {
	data, err := ioutil.ReadFile(kubeconfigPath)
	if err != nil {
		return nil, fmt.Errorf("read kubeconfig: %v", err)
	}

	var config k8s.Config
	if err := yaml.Unmarshal(data, &config); err != nil {
		return nil, fmt.Errorf("unmarshal kubeconfig: %v", err)
	}
	c, err := k8s.NewClient(&config)
	if err != nil {
		return nil, err
	}
	return c.AppsV1Beta1(), nil
}
