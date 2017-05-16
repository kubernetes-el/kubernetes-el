package main

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"log"
	"strings"

	"github.com/ericchiang/k8s"
	"github.com/ericchiang/k8s/api/v1"
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
	fmt.Fprintf(w, "])")
	return nil
}

func podSexpr(w io.Writer, p *v1.Pod) error {
	var labels bytes.Buffer
	for k, v := range p.Metadata.Labels {
		fmt.Fprintf(&labels, "(\"%s\" . \"%s\") ", k, v)
	}

	var containerStatuses bytes.Buffer
	for _, v := range p.Status.ContainerStatuses {
		containerStatuses.WriteString("(")
		fmt.Fprintf(&containerStatuses, "(name . \"%s\") ", *v.Name)
		fmt.Fprintf(&containerStatuses, "(image . \"%s\") ", *v.Image)
		fmt.Fprintf(&containerStatuses, "(restartCount . %d)", *v.RestartCount)
		containerStatuses.WriteString(") ")
	}

	startTime := ""

	fmt.Fprintf(w, "(")
	// MetaData
	fmt.Fprintf(w, "(metaData . ")
	fmt.Fprintf(w, "(name . \"%s\") ", *p.Metadata.Name)
	fmt.Fprintf(w, "(namespace . \"%s\") ", *p.Metadata.Namespace)
	fmt.Fprintf(w, "(labels . (%s))", strings.TrimSpace(labels.String()))
	fmt.Fprintf(w, ") ")
	// Status
	fmt.Fprintf(w, "(status . ")
	// ContainerStatuses
	fmt.Fprintf(w, "(containerStatuses . (%s)) ", strings.TrimSpace(containerStatuses.String()))
	fmt.Fprintf(w, "(hostIP . \"%s\") ", *p.Status.HostIP)
	fmt.Fprintf(w, "(podIP . \"%s\") ", *p.Status.PodIP)
	fmt.Fprintf(w, "(startTime . \"%s\") ", startTime)
	fmt.Fprintf(w, "(phase . \"%s\")", *p.Status.Phase)
	fmt.Fprintf(w, ")")
	fmt.Fprintf(w, ")")
	return nil
}
