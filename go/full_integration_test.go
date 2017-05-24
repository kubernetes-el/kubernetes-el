package main

import (
	"bytes"
	"fmt"
	"testing"
)

func TestFullIntegration(t *testing.T) {
	t.Skip("integration test")

	c, err := loadDefaultClient()
	if err != nil {
		t.Error(err)
	}
	var b bytes.Buffer
	err = listPods(&b, c)
	if err != nil {
		t.Error(err)
	}
	fmt.Printf("%s\n", b.String())
}
