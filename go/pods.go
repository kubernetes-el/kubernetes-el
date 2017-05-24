package main

import (
	"context"
	"io"
	"log"

	"github.com/ericchiang/k8s"
	api "github.com/ericchiang/k8s/api/v1"
	"github.com/kalmanb/sexpr"
)

type podsUpdate struct {
	Type      string     `json:"type"`
	Operation string     `json:"operation"`
	Data      []*api.Pod `json:"data"`
}

func listPods(w io.Writer, client *k8s.Client) error {
	ctx := context.TODO()
	pods, err := client.CoreV1().ListPods(ctx, client.Namespace) // k8s.AllNamespaces for all
	if err != nil {
		log.Fatal(err)
	}

	p := podsUpdate{
		Type:      "pod",
		Operation: "upsert",
		Data:      pods.Items,
	}

	e := sexpr.NewEncoder(w)
	return e.Encode(p)
}
