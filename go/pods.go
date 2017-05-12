package main

import (
	"context"
	"fmt"
	"log"

	"github.com/ericchiang/k8s"
)

type Pod struct {
	name string
}

func getPods(client *k8s.Client) {
	ctx := context.TODO()
	pods, err := client.CoreV1().ListPods(ctx, k8s.AllNamespaces)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Printf("Here's the pods\n")
	for _, item := range pods.Items {
		fmt.Printf("name=%s status=%q \n", *item.Metadata.Name, *item.Status.Phase)
	}
}
