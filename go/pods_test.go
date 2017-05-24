package main

import (
	"context"
	"errors"
	"testing"
	"time"

	"github.com/ericchiang/k8s"
	api "github.com/ericchiang/k8s/api/v1"
	meta "github.com/ericchiang/k8s/apis/meta/v1"
	"github.com/kalmanb/sexp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

type k8sMock struct {
	mock.Mock
}

func (c *k8sMock) ListPods(ctx context.Context, n string, options ...k8s.Option) (*api.PodList, error) {
	args := c.Called(ctx, n)
	return nil, args.Error(1)
}

func TestSched(t *testing.T) {
	// var b bytes.Buffer
	// c := newPodClient(k, &sync.Mutex{}, &b)
	// t.Fail()  FIXME
}

func TestDiffUpserts(t *testing.T) {
	a := &api.Pod{Metadata: &meta.ObjectMeta{Uid: strPtr("a")}}
	b := &api.Pod{Metadata: &meta.ObjectMeta{Uid: strPtr("b")}}
	c := &api.Pod{Metadata: &meta.ObjectMeta{Uid: strPtr("c")}}
	d := &api.Pod{Metadata: &meta.ObjectMeta{Uid: strPtr("d")}}
	dUpdated := &api.Pod{Metadata: &meta.ObjectMeta{Uid: strPtr("d"), Name: strPtr("d")}}
	e := &api.Pod{Metadata: &meta.ObjectMeta{Uid: strPtr("e")}}

	client := newPodClient(nil, nil, "", 0)
	client.pods["b"] = b
	client.pods["c"] = c
	client.pods["d"] = d

	p := []*api.Pod{a, b, dUpdated, e}

	diff := client.podUpserts(p)

	// Should have a, dUpdated, e
	assert.Equal(t, 3, len(diff))
	assert.Equal(t, diff[0], a)
	assert.Equal(t, diff[1], dUpdated)
	assert.Equal(t, diff[2], e)
}

func TestDiffDeletes(t *testing.T) {
	c := newPodClient(nil, nil, "", 0)
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

func TestListPodError(t *testing.T) {
	k8s := new(k8sMock)
	k8s.On("ListPods", mock.Anything, mock.Anything).Return(nil, errors.New("oh no"))
	w := make(chan []byte)
	c := newPodClient(k8s, w, "", 0)
	c.setInterval(time.Millisecond)
	c.sched()
	res := <-w
	assert.Contains(t, string(res), "Could not get pods")
	res = <-w
	assert.Contains(t, string(res), "Could not get pods", "should not send upsert on failure")
}

func TestPodSexp(t *testing.T) {
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
			HostIP: strPtr("123"),
			PodIP:  strPtr("456"),
			Phase:  strPtr("Starting"),
		},
	}
	result, err := sexp.Marshal(&pod)
	if err != nil {
		t.Error(err)
	}
	expected := "((metadata . ((name . \"aname\")(namespace . \"space\")(labels . ((\"t1\" . \"v1\")))))(status . ((phase . \"Starting\")(hostIP . \"123\")(podIP . \"456\")(containerStatuses . [((name . \"thename\")(restartCount . 10)(image . \"aimage\"))]))))"
	assert.Equal(t, expected, string(result))
}

func strPtr(s string) *string { return &s }
