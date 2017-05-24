package main

import (
	"flag"
	"os"
)

func main() {
	flag.Parse()

	c := newClient(os.Stdout, os.Stdin)
	exitCode := c.run()
	os.Exit(exitCode)
}
