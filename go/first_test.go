package main

import "testing"

func TestFirst(t *testing.T) {
	c, err := loadDefaultClient()
	if err != nil {
		t.Error(err)
	}
	getPods(c)
}
