package main

import (
	"context"
	"reflect"
	"time"

	apps "github.com/ericchiang/k8s/apis/apps/v1beta1"
	"github.com/kalmanb/sexp"
)

type deploymentUpdate struct {
	Type      string             `json:"type"`
	Operation string             `json:"operation"`
	Data      []*apps.Deployment `json:"data"`
}

type deploymentDeletes struct {
	Type      string   `json:"type"`
	Operation string   `json:"operation"`
	Data      []string `json:"data"`
}

type deploymentClient struct {
	appsClient  appsClient
	writer      chan []byte
	interval    time.Duration
	namespace   string
	deployments map[string]*apps.Deployment
}

func newDeploymentClient(c appsClient, writer chan []byte, namespace string, interval int) deploymentClient {
	return deploymentClient{
		appsClient:  c,
		writer:      writer,
		interval:    time.Second * time.Duration(interval),
		namespace:   namespace,
		deployments: map[string]*apps.Deployment{},
	}
}

func (c *deploymentClient) setNamespace(n string) {
	c.namespace = n
}

func (c *deploymentClient) setInterval(d time.Duration) {
	c.interval = d
}

func (c *deploymentClient) sched() {
	go func() {
		for {
			c.run()
			timer := time.NewTimer(c.interval)
			<-timer.C
		}
	}()
}

func (c *deploymentClient) run() {
	current, err := c.appsClient.ListDeployments(context.Background(), c.namespace)
	if err != nil {
		c.writer <- errorSexp("Could not get deployments", err)
		return
	}

	// Diff
	upserts := c.upserts(current.Items)
	c.removeUnusedData(upserts)
	p := deploymentUpdate{
		Type:      "deployment",
		Operation: "upsert",
		Data:      upserts,
	}
	res, err := sexp.Marshal(p)
	if err != nil {
		c.writer <- errorSexp("Could not not marshal upserts", err)
		return
	}
	c.writer <- res

	for _, v := range upserts {
		c.deployments[*v.Metadata.Uid] = v
	}

	// Delete
	deletes := c.deletes(current.Items)
	d := deploymentDeletes{
		Type:      "deployment",
		Operation: "delete",
		Data:      deletes,
	}
	res, err = sexp.Marshal(d)
	if err != nil {
		c.writer <- errorSexp("Could not not marshal deletes", err)
		return
	}
	c.writer <- res

	for _, uid := range deletes {
		delete(c.deployments, uid)
	}
}

func (c deploymentClient) upserts(deployments []*apps.Deployment) []*apps.Deployment {
	ids := make(map[string]bool)
	for _, v := range c.deployments {
		ids[*v.Metadata.Uid] = true
	}

	var res []*apps.Deployment
	for _, v := range deployments {
		lookup := c.deployments[*v.Metadata.Uid]

		if lookup == nil {
			// Not Present
			res = append(res, v)
		} else if !reflect.DeepEqual(lookup, v) {
			// Updated
			res = append(res, v)
		}
	}
	return res
}

func (c deploymentClient) deletes(deployments []*apps.Deployment) []string {
	ids := make(map[string]bool)
	for _, v := range deployments {
		ids[*v.Metadata.Uid] = true
	}

	var res []string
	for k, v := range c.deployments {
		if !ids[k] {
			res = append(res, *v.Metadata.Uid)
		}
	}
	return res
}

func (c deploymentClient) removeUnusedData(deployments []*apps.Deployment) []*apps.Deployment {
	// Option 1 - remove data
	// for _, pod := range pods {
	// 	pod.Spec = nil
	// 	pod.Metadata.GenerateName = nil
	// }
	// return pods
	return deployments

	// FIXME - use this
	// Option 2 - create new pod with data
	// var res []*apps.Deployment
	// for _, pod := range pods {
	//	new := &apps.Deployment{
	//		Metadata: &meta.ObjectMeta{
	//			Name:      pod.Metadata.Name,
	//			Namespace: pod.Metadata.Namespace,
	//          ...
	//		},
	//		Status: &apps.DeploymentStatus{
	//			Phase: pod.Status.Phase,
	//          ...
	//		},
	//      ...
	//	}
	//	res = append(res, new)
	// }
	// return res
}
