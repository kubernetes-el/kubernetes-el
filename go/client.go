package main

import "sync"

type client struct {
	writer chan []byte
}

func newClient(writer chan []byte) *client {
	return &client{writer}
}

func (c *client) run(namespace string, interval int) int {
	// s := bufio.NewScanner(c.in)
	// for s.Scan() {
	//	command := s.Text()
	//	fmt.Printf("go command: %s\n", command)
	// }

	wg := sync.WaitGroup{}
	wg.Add(1)

	// client, err := loadDefaultClient()
	// if err != nil {
	// 	c.writer <- errorSexp("could not load k8s client", err)
	// 	return -1
	// }

	// podClient := newPodClient(client, c.writer, namespace, interval)
	// podClient.sched()

	appsClient, err := loadDefaultAppsClient()
	if err != nil {
		c.writer <- errorSexp("could not load k8s client", err)
		return -1
	}

	deploymentClient := newDeploymentClient(appsClient, c.writer, namespace, interval)
	deploymentClient.sched()

	wg.Wait()
	return -1
}
