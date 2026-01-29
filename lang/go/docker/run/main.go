package main

import (
	"context"
	"io"
	"log"
	"os"

	"github.com/moby/moby/api/types/container"
	"github.com/moby/moby/client"
)

const imageName = "my-custom-image:latest"

func main() {
	ctx := context.Background()

	cli, err := client.New(client.FromEnv, client.WithAPIVersionFromEnv())
	if err != nil {
		log.Fatal(err)
	}

	config := &container.Config{
		Image: imageName,
	}
	hostConfig := &container.HostConfig{
		// AutoRemove: true, // docker run --rm
	}
	options := client.ContainerCreateOptions{
		Config:     config,
		HostConfig: hostConfig,
	}

	resp, err := cli.ContainerCreate(ctx, options)
	if err != nil {
		log.Fatal(err)
	}

	id := resp.ID
	log.Printf("Container created with ID: %s", id)

	result, err := cli.ContainerStart(ctx, id, client.ContainerStartOptions{})
	if err != nil {
		log.Fatal(err)
	}

	log.Printf("Container started: %v", result)

	out, err := cli.ContainerLogs(ctx, id, client.ContainerLogsOptions{
		ShowStdout: true,
		ShowStderr: true,
		Follow:     true,
	})
	if err != nil {
		log.Fatal(err)
	}
	defer out.Close()

	_, err = io.Copy(os.Stdout, out)
	if err != nil {
		log.Fatal(err)
	}

	log.Println("Container execution completed")
}
