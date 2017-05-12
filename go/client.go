package main

import (
	"bufio"
	"fmt"
	"io"
)

type client struct {
	out io.Writer
	in  io.Reader
}

func (c client) run() int {
	s := bufio.NewScanner(c.in)
	for s.Scan() {
		command := s.Text()
		fmt.Printf("go command: %s\n", command)
	}

	w := bufio.NewWriter(c.out)
	_, err := w.WriteString("some stuff\n")
	if err != nil {
		panic(err)
	}
	if err != w.Flush() {
		panic(err)
	}

	return -1
}
