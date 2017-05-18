package main

import (
	"context"
	"fmt"
	"io"
	"log"

	"github.com/ericchiang/k8s"
)

func listPods(w io.Writer, client *k8s.Client) error {
	ctx := context.TODO()
	pods, err := client.CoreV1().ListPods(ctx, client.Namespace) // k8s.AllNamespaces for all
	if err != nil {
		log.Fatal(err)
	}

	fmt.Fprintf(w, "((type . \"pod\") (operation . \"upsert\") (data . [")
	for _, item := range pods.Items {
		err := podSexpr(w, item)
		if err != nil {
			panic("FIXME")
		}
	}
	fmt.Fprintf(w, "]))")
	return nil
}
