package main

import (
	"flag"
	"os"
)

func main() {
	flag.Parse()

	c := client{os.Stdout, os.Stdin}
	exitCode := c.run()
	os.Exit(exitCode)
}
