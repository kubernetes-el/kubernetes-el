package main

import (
	"context"
	"reflect"
	"time"

	api "github.com/ericchiang/k8s/api/v1"
	"github.com/kalmanb/sexp"
)

type podsUpdate struct {
	Type      string     `json:"type"`
	Operation string     `json:"operation"`
	Data      []*api.Pod `json:"data"`
}

type podsDeletes struct {
	Type      string   `json:"type"`
	Operation string   `json:"operation"`
	Data      []string `json:"data"`
}

type podClient struct {
	k8sClient k8sClient
	writer    chan []byte
	interval  time.Duration
	namespace string
	pods      map[string]*api.Pod
}

func newPodClient(k k8sClient, writer chan []byte, namespace string, interval int) podClient {
	return podClient{
		k8sClient: k,
		writer:    writer,
		interval:  time.Second * time.Duration(interval),
		namespace: namespace,
		pods:      map[string]*api.Pod{},
	}
}

func (c *podClient) setNamespace(n string) {
	c.namespace = n
}

func (c *podClient) setInterval(d time.Duration) {
	c.interval = d
}

func (c *podClient) sched() {
	go func() {
		for {
			c.run()
			timer := time.NewTimer(c.interval)
			<-timer.C
		}
	}()
}

func (c *podClient) run() {
	currentPods, err := c.k8sClient.ListPods(context.Background(), c.namespace)
	if err != nil {
		c.writer <- errorSexp("Could not get pods", err)
		return
	}

	// Diff
	upserts := c.upserts(currentPods.Items)
	c.removeUnusedData(upserts)
	p := podsUpdate{
		Type:      "pod",
		Operation: "upsert",
		Data:      upserts,
	}
	res, err := sexp.Marshal(p)
	if err != nil {
		c.writer <- errorSexp("Could not not marshal pod upserts", err)
		return
	}
	c.writer <- res

	for _, pod := range upserts {
		c.pods[*pod.Metadata.Uid] = pod
	}

	// Delete
	deletes := c.deletes(currentPods.Items)
	pd := podsDeletes{
		Type:      "pod",
		Operation: "delete",
		Data:      deletes,
	}
	res, err = sexp.Marshal(pd)
	if err != nil {
		c.writer <- errorSexp("Could not not marshal pod deletes", err)
		return
	}
	c.writer <- res

	for _, uid := range deletes {
		delete(c.pods, uid)
	}
}

func (c podClient) upserts(p []*api.Pod) []*api.Pod {
	podsIds := make(map[string]bool)
	for _, p := range c.pods {
		podsIds[*p.Metadata.Uid] = true
	}

	var pods []*api.Pod
	for _, pod := range p {
		lookup := c.pods[*pod.Metadata.Uid]

		if lookup == nil {
			// Not Present
			pods = append(pods, pod)
		} else if !reflect.DeepEqual(lookup, pod) {
			// Updated
			pods = append(pods, pod)
		}
	}
	return pods
}

func (c podClient) deletes(pods []*api.Pod) []string {
	podsIds := make(map[string]bool)
	for _, p := range pods {
		podsIds[*p.Metadata.Uid] = true
	}

	var ids []string
	for i, p := range c.pods {
		if !podsIds[i] {
			ids = append(ids, *p.Metadata.Uid)
		}
	}
	return ids
}

func (c podClient) removeUnusedData(pods []*api.Pod) []*api.Pod {
	// Option 1 - remove data
	for _, pod := range pods {
		pod.Spec = nil
		pod.Metadata.GenerateName = nil
	}
	return pods

	// Option 2 - create new pod with data
	// var res []*api.Pod
	// for _, pod := range pods {
	//	new := &api.Pod{
	//		Metadata: &meta.ObjectMeta{
	//			Name:      pod.Metadata.Name,
	//			Namespace: pod.Metadata.Namespace,
	//          ...
	//		},
	//		Status: &api.PodStatus{
	//			Phase: pod.Status.Phase,
	//          ...
	//		},
	//      ...
	//	}
	//	res = append(res, new)
	// }
	// return res
}
