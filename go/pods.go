package main

import (
	"context"
	"io"
	"sync"
	"time"

	"github.com/ericchiang/k8s"
	api "github.com/ericchiang/k8s/api/v1"
	"github.com/kalmanb/sexpr"
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
	k8sClient *k8s.Client
	m         *sync.Mutex // FIXME remove - use channel instead
	w         io.Writer   // FIXME - remove
	pods      map[string]*api.Pod
}

func newPodClient(k *k8s.Client, m *sync.Mutex, w io.Writer) podClient {
	return podClient{
		k, m, w, map[string]*api.Pod{},
	}
}

func (c podClient) sched() {
	go func() {
		for {
			// var b bytes.Buffer
			e := sexpr.NewEncoder(c.w)

			currentPods, err := c.listPods()
			if err != nil {
				writeError(c.w, "Could not get pods", err)
			}

			// Diff
			upserts := c.podUpserts(currentPods)
			p := podsUpdate{
				Type:      "pod",
				Operation: "upsert",
				Data:      upserts,
			}
			c.m.Lock()
			err = e.Encode(p)
			if err != nil {
				// FIXME
			}
			c.m.Unlock()

			// Delete
			deletes := c.podDeletes(currentPods)
			pd := podsDeletes{
				Type:      "pod",
				Operation: "delete",
				Data:      deletes,
			}
			c.m.Lock()
			err = e.Encode(pd)
			if err != nil {
				// FIXME
			}
			c.m.Unlock()

			// Run again later
			timer := time.NewTimer(time.Second * 10)
			<-timer.C
		}
	}()
}

func (c podClient) listPods() ([]*api.Pod, error) {
	ctx := context.TODO()
	l, err := c.k8sClient.CoreV1().ListPods(ctx, c.k8sClient.Namespace) // k8s.AllNamespaces for all
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
		} else if lookup != pod {
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
