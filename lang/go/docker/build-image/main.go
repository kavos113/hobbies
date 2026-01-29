package main

import (
	"context"
	"io"
	"log"
	"os"
	"path/filepath"

	"github.com/moby/moby/client"
)

var imageFileName = filepath.Join("image", "image.tar")

func main() {
	ctx := context.Background()

	cli, err := client.New(client.FromEnv, client.WithAPIVersionFromEnv())
	if err != nil {
		log.Fatal(err)
	}

	f, err := os.Open(imageFileName)
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	options := client.ImageBuildOptions{
		Tags:       []string{"my-custom-image:latest"},
		Remove:     true,
		Dockerfile: "Dockerfile",
	}
	resp, err := cli.ImageBuild(ctx, f, options)
	if err != nil {
		log.Fatal(err)
	}
	defer resp.Body.Close()

	_, err = io.Copy(os.Stdout, resp.Body)
	if err != nil {
		log.Fatal(err)
	}

	log.Println("Image built successfully")
}
