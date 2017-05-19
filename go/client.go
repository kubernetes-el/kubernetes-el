package main

import (
	"io"
	"sync"
)

type client struct {
	w         io.Writer
	r         io.Reader
	wg        sync.WaitGroup
	m         sync.Mutex
	podClient podClient
}

func newClient(out io.Writer, in io.Reader) *client {
	return &client{out, in, sync.WaitGroup{}, sync.Mutex{}, podClient{}}
}

func (c *client) run() int {
	// s := bufio.NewScanner(c.in)
	// for s.Scan() {
	//	command := s.Text()
	//	fmt.Printf("go command: %s\n", command)
	// }

	c.wg.Add(1)

	client, err := loadDefaultClient()
	if err != nil {
		panic("FIXME")
	}
	podClient := newPodClient(client, &c.m, c.w)
	podClient.sched()

	c.wg.Wait()
	return -1
}
