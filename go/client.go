package main

import (
	"bufio"
	"io"
)

type client struct {
	out io.Writer
	in  io.Reader
}

func (c client) run() int {
	r := bufio.NewReader(c.in)
	command, err := r.ReadString('|')
	if err != nil {
		panic(err)
	}

	w := bufio.NewWriter(c.out)
	_, err = w.WriteString(command)
	if err != nil {
		panic(err)
	}
	if err != w.Flush() {
		panic(err)
	}

	return -1
}
