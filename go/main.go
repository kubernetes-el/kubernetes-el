package main

import (
	"flag"
	"os"
)

func main() {
	var namespace = flag.String("namespace", "", "Namespace that you want to use, empty \"\" for All namespaces")
	var interval = flag.Int("interval", 10, "Refresh interveal in seconds, defaults to 10 secounds")
	flag.Parse()

	writer := make(chan []byte)
	go func() {
		for {
			b := <-writer
			_, err := os.Stdout.Write(b)
			if err != nil {
				panic("Could not write to stdout")
			}
		}
	}()

	c := newClient(writer)
	exitCode := c.run(*namespace, *interval)
	os.Exit(exitCode)
}
