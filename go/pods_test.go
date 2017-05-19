package main

import (
	"testing"

	api "github.com/ericchiang/k8s/api/v1"
	meta "github.com/ericchiang/k8s/apis/meta/v1"
	"github.com/kalmanb/sexpr"
	"github.com/stretchr/testify/assert"
)

func TestSched(t *testing.T) {
	// var b bytes.Buffer
	// c := newPodClient(k, &sync.Mutex{}, &b)
	t.Fail()
}
func TestDiffDeletes(t *testing.T) {
	c := newPodClient(nil, nil, nil)
	c.pods["a"] = &api.Pod{Metadata: &meta.ObjectMeta{Uid: strPtr("a")}}
	c.pods["b"] = &api.Pod{Metadata: &meta.ObjectMeta{Uid: strPtr("b")}}
	c.pods["c"] = &api.Pod{Metadata: &meta.ObjectMeta{Uid: strPtr("c")}}

	p := []*api.Pod{
		&api.Pod{Metadata: &meta.ObjectMeta{Uid: strPtr("b")}},
		&api.Pod{Metadata: &meta.ObjectMeta{Uid: strPtr("d")}},
	}

	diff := c.podDeletes(p)

	assert.Contains(t, diff, "a", "c")
	assert.NotContains(t, diff, "b", "d")
}

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
	result, err := sexpr.Marshal(&pod)
	if err != nil {
		t.Error(err)
	}
	expected := "((metadata . ((name . \"aname\")(namespace . \"space\")(labels . ((\"t1\" . \"v1\")))))(status . ((phase . \"Starting\")(hostIP . \"123\")(podIP . \"456\")(startTime . \"1970-01-01T12:00:00+12:00\")(containerStatuses . [((name . \"thename\")(restartCount . 10)(image . \"aimage\"))]))))"
	assert.Equal(t, expected, string(result))
}

func strPtr(s string) *string { return &s }
