package main

import (
	"bytes"
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

type podClient struct {
	k8sClient *k8s.Client
	m         *sync.Mutex
	w         io.Writer
	items     []api.Pod
}

func newPodClient(k *k8s.Client, m *sync.Mutex, w io.Writer) podClient {
	return podClient{
		k, m, w, []api.Pod{},
	}
}

func (c podClient) sched() {
	go func() {
		for {
			var b bytes.Buffer
			err := c.listPods()
			if err != nil {
				writeError(c.w, "Could not get pods", err)
			}

			// Diff

			c.m.Lock()
			_, err = b.WriteTo(c.w)
			if err != nil {
				panic("could not write to std out")
			}
			c.m.Unlock()

			// Run again later
			timer := time.NewTimer(time.Second * 10)
			<-timer.C
		}
	}()
}

func (c podClient) listPods() error {
	ctx := context.TODO()
	pods, err := c.k8sClient.CoreV1().ListPods(ctx, c.k8sClient.Namespace) // k8s.AllNamespaces for all
	if err != nil {
		return err
	}

	p := podsUpdate{
		Type:      "pod",
		Operation: "upsert",
		Data:      pods.Items,
	}

	e := sexpr.NewEncoder(c.w)
	return e.Encode(p)
}
