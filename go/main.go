package main

import (
	"bufio"
	"flag"
	"os"
)

func main() {
	var namespace = flag.String("namespace", "", "Namespace that you want to use, empty \"\" for All namespaces")
	var interval = flag.Int("interval", 10, "Refresh interveal in seconds, defaults to 10 secounds")
	flag.Parse()

	out := bufio.NewWriter(os.Stdout)
	writer := make(chan []byte)
	go func() {
		for {
			b := <-writer
			b = append(b, byte('\n'))
			_, err := out.Write(b)
			if err != nil {
				panic("Could not write to stdout")
			}
			err = out.Flush()
			if err != nil {
				panic("Could not write to stdout")
			}
		}
	}()

	c := newClient(writer)
	exitCode := c.run(*namespace, *interval)
	os.Exit(exitCode)
}
