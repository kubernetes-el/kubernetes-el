package main

import (
	"bytes"
	"testing"

	api "github.com/ericchiang/k8s/api/v1"
	meta "github.com/ericchiang/k8s/apis/meta/v1"
	"github.com/stretchr/testify/assert"
)

func TestPodSexpr(t *testing.T) {
	pod := api.Pod{
		Metadata: &meta.ObjectMeta{
			Name:      strPtr("aname"),
			Namespace: strPtr("space"),
			Labels:    map[string]string{"t1": "v1"},
		},
		Status: &api.PodStatus{
			ContainerStatuses: []*api.ContainerStatus{
				&api.ContainerStatus{
					Name:         strPtr("thename"),
					Image:        strPtr("aimage"),
					RestartCount: &[]int32{10}[0],
				}},
			HostIP:    strPtr("123"),
			PodIP:     strPtr("456"),
			StartTime: &meta.Time{},
			Phase:     strPtr("Starting"),
		},
	}
	var result bytes.Buffer
	err := podSexpr(&result, &pod)
	if err != nil {
		t.Error(err)
	}

	var b bytes.Buffer
	b.WriteString("(")
	// Meta Data
	b.WriteString("(metaData . ")
	b.WriteString("(name . \"aname\")")
	b.WriteString(" (namespace . \"space\")")
	b.WriteString(" (labels . ((\"t1\" . \"v1\")))")
	b.WriteString(") ")
	// Status
	b.WriteString("(status . ")
	// Container Status
	b.WriteString("(containerStatuses . ((")
	b.WriteString("(name . \"thename\")")
	b.WriteString(" (image . \"aimage\")")
	b.WriteString(" (restartCount . 10)")
	b.WriteString("))) ")
	b.WriteString("(hostIP . \"123\")")
	b.WriteString(" (podIP . \"456\")")
	b.WriteString(" (startTime . \"\")")
	b.WriteString(" (phase . \"Starting\")")
	b.WriteString(")")
	b.WriteString(")")
	assert.Equal(t, b.String(), result.String())
}

func strPtr(s string) *string { return &s }
