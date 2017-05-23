package main

import (
	"flag"
	"os"
)

func main() {
	flag.Parse()

	writer := make(chan []byte)
	go func() {
		for {
			b := <-writer
			os.Stdout.Write(b)
		}
	}()

	c := newClient(writer)
	exitCode := c.run()
	os.Exit(exitCode)
}
