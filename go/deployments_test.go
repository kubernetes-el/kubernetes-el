package main

import (
	"context"
	"errors"
	"testing"

	"github.com/ericchiang/k8s"
	extns "github.com/ericchiang/k8s/apis/extensions/v1beta1"
	meta "github.com/ericchiang/k8s/apis/meta/v1"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

type extnsClientMock struct {
	mock.Mock
}

func (c *extnsClientMock) ListDeployments(ctx context.Context, n string, options ...k8s.Option) (*extns.DeploymentList, error) {
	args := c.Called(ctx, n)
	if args.Get(0) == nil {
		return nil, args.Error(1)
	}
	return args.Get(0).(*extns.DeploymentList), args.Error(1)
}

func TestDeployementDiffUpserts(t *testing.T) {
	a := createDeployment("a")
	b := createDeployment("b")
	c := createDeployment("c")
	d := createDeployment("d")
	dUpdated := &extns.Deployment{Metadata: &meta.ObjectMeta{Uid: strPtr("d"), Name: strPtr("d")}}
	e := createDeployment("e")

	client := newDeploymentClient(nil, nil, "", 0)
	client.deployments["b"] = b
	client.deployments["c"] = c
	client.deployments["d"] = d

	p := []*extns.Deployment{a, b, dUpdated, e}

	diff := client.upserts(p)

	// Should have a, dUpdated, e
	assert.Equal(t, 3, len(diff))
	assert.Equal(t, diff[0], a)
	assert.Equal(t, diff[1], dUpdated)
	assert.Equal(t, diff[2], e)
}

func TestDeployementDiffDeletes(t *testing.T) {
	c := newDeploymentClient(nil, nil, "", 0)
	c.deployments["a"] = createDeployment("a")
	c.deployments["b"] = createDeployment("b")
	c.deployments["c"] = createDeployment("c")

	p := []*extns.Deployment{
		createDeployment("b"),
		createDeployment("d"),
	}

	diff := c.deletes(p)

	assert.Contains(t, diff, "a", "c")
	assert.NotContains(t, diff, "b", "d")
}

func TestDeployementListError(t *testing.T) {
	client := new(extnsClientMock)
	client.On("ListDeployments", mock.Anything, mock.Anything).Return(nil, errors.New("oh no"))
	w := make(chan []byte, 10)
	c := newDeploymentClient(client, w, "", 0)
	c.run()

	m := <-w
	assert.Contains(t, string(m), "Could not get deployments")
	select {
	case _ = <-w:
		assert.Fail(t, "should only get an error message, no other messages")
		break
	default:
		break
	}
}

func TestDeployementFullRun(t *testing.T) {
	client := new(extnsClientMock)
	a := &extns.DeploymentList{
		Items: []*extns.Deployment{createDeployment("a")},
	}
	b := &extns.DeploymentList{
		Items: []*extns.Deployment{createDeployment("b")},
	}

	w := make(chan []byte, 10)
	c := newDeploymentClient(client, w, "", 0)

	mockCall := client.On("ListDeployments", mock.Anything, mock.Anything).Return(a, nil)
	c.run()

	m := <-w
	assert.Contains(t, string(m), "operation . \"upsert\"")
	assert.Contains(t, string(m), "uid . \"a\"")
	m = <-w
	assert.Contains(t, string(m), "operation . \"delete\"")
	assert.Contains(t, string(m), "data . nil")

	mockCall.ReturnArguments = mock.Arguments{b, nil}
	c.run()

	m = <-w
	assert.Contains(t, string(m), "operation . \"upsert\"")
	assert.Contains(t, string(m), "uid . \"b\"")
	m = <-w
	assert.Contains(t, string(m), "operation . \"delete\"")
	assert.Contains(t, string(m), "data . [\"a\"]")
}

func createDeployment(uid string) *extns.Deployment {
	return &extns.Deployment{
		Metadata: &meta.ObjectMeta{
			Uid: strPtr(uid),
		},
	}
}
