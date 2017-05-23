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
	pods      map[string]*api.Pod
}

func newPodClient(k k8sClient, writer chan []byte) podClient {
	return podClient{
		k8sClient: k,
		writer:    writer,
		interval:  time.Second * 10,
		pods:      map[string]*api.Pod{},
	}
}

func (c *podClient) setInterval(d time.Duration) {
	c.interval = d
}

func (c podClient) sched() {
	go func() {
		for {
			currentPods, err := c.listPods("namespace") // FIXME
			if err != nil {
				c.writer <- writeError("Could not get pods", err)
				continue
			}

			// Diff
			upserts := c.podUpserts(currentPods)
			p := podsUpdate{
				Type:      "pod",
				Operation: "upsert",
				Data:      upserts,
			}
			res, err := sexp.Marshal(p)
			if err != nil {
				// FIXME
			}
			c.writer <- res
			c.writer <- []byte("\n")

			for _, pod := range upserts {
				c.pods[*pod.Metadata.Uid] = pod
			}

			// Delete
			deletes := c.podDeletes(currentPods)
			pd := podsDeletes{
				Type:      "pod",
				Operation: "delete",
				Data:      deletes,
			}
			res, err = sexp.Marshal(pd)
			if err != nil {
				// FIXME
			}
			c.writer <- res
			c.writer <- []byte("\n")

			for _, uid := range deletes {
				delete(c.pods, uid)
			}

			// Run again later
			timer := time.NewTimer(c.interval)
			<-timer.C
		}
	}()
}

func (c podClient) listPods(namespace string) ([]*api.Pod, error) {
	ctx := context.TODO()
	l, err := c.k8sClient.ListPods(ctx, namespace)
	if err != nil {
		return nil, err
	}
	return l.Items, nil
}

func (c podClient) podUpserts(p []*api.Pod) []*api.Pod {
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

func (c podClient) podDeletes(pods []*api.Pod) []string {
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
