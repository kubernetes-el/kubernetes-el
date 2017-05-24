package main

import (
	"bytes"
	"io"
	"sync"
	"time"

	"github.com/ericchiang/k8s"
)

type client struct {
	out io.Writer
	in  io.Reader
	wg  sync.WaitGroup
	m   sync.Mutex
}

func newClient(out io.Writer, in io.Reader) *client {
	return &client{out, in, sync.WaitGroup{}, sync.Mutex{}}
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
	go listPodsSched(client, c.out, &c.m)

	c.wg.Wait()
	return -1
}

func listPodsSched(client *k8s.Client, w io.Writer, m *sync.Mutex) {
	for {
		var b bytes.Buffer
		err := listPods(&b, client)
		if err != nil {
			panic("FIXME")
		}

		m.Lock()
		_, err = b.WriteTo(w)
		if err != nil {
			panic("FIXME")
		}
		m.Unlock()

		// Run again later
		timer := time.NewTimer(time.Second * 10)
		<-timer.C
	}
}
