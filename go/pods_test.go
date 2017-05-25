package main

import (
	"context"
	"errors"
	"testing"

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
	if args.Get(0) == nil {
		return nil, args.Error(1)
	}
	return args.Get(0).(*api.PodList), args.Error(1)
}

func TestDiffUpserts(t *testing.T) {
	a := createPod("a")
	b := createPod("b")
	c := createPod("c")
	d := createPod("d")
	dUpdated := &api.Pod{Metadata: &meta.ObjectMeta{Uid: strPtr("d"), Name: strPtr("d")}}
	e := createPod("e")

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
	c.pods["a"] = createPod("a")
	c.pods["b"] = createPod("b")
	c.pods["c"] = createPod("c")

	p := []*api.Pod{
		createPod("b"),
		createPod("d"),
	}

	diff := c.podDeletes(p)

	assert.Contains(t, diff, "a", "c")
	assert.NotContains(t, diff, "b", "d")
}

func TestListPodError(t *testing.T) {
	k8s := new(k8sMock)
	k8s.On("ListPods", mock.Anything, mock.Anything).Return(nil, errors.New("oh no"))
	w := make(chan []byte, 10)
	c := newPodClient(k8s, w, "", 0)
	c.run()

	m := <-w
	assert.Contains(t, string(m), "Could not get pods")
	select {
	case _ = <-w:
		assert.Fail(t, "should only get an error message, no other messages")
		break
	default:
		break
	}
}

func TestFullRun(t *testing.T) {
	k8s := new(k8sMock)
	podsA := &api.PodList{
		Items: []*api.Pod{createPod("a")},
	}
	podsB := &api.PodList{
		Items: []*api.Pod{createPod("b")},
	}

	w := make(chan []byte, 10)
	c := newPodClient(k8s, w, "", 0)

	mockCall := k8s.On("ListPods", mock.Anything, mock.Anything).Return(podsA, nil)
	c.run()

	m := <-w
	assert.Contains(t, string(m), "operation . \"upsert\"")
	assert.Contains(t, string(m), "uid . \"a\"")
	m = <-w
	assert.Contains(t, string(m), "operation . \"delete\"")
	assert.Contains(t, string(m), "data . nil")

	mockCall.ReturnArguments = mock.Arguments{podsB, nil}
	c.run()

	m = <-w
	assert.Contains(t, string(m), "operation . \"upsert\"")
	assert.Contains(t, string(m), "uid . \"b\"")
	m = <-w
	assert.Contains(t, string(m), "operation . \"delete\"")
	assert.Contains(t, string(m), "data . [\"a\"]")
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

func createPod(uid string) *api.Pod {
	return &api.Pod{
		Metadata: &meta.ObjectMeta{
			Uid: strPtr(uid),
		},
	}
}
