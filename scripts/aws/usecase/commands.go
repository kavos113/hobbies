package usecase

import (
	"fmt"
	"os"
	"path/filepath"
	"time"

	"github.com/kavos113/hobbies/scripts/aws/client"
)

type Commands struct {
	cw client.CloudWatchClient
	s3 client.S3Client
}

func NewCommands(cw client.CloudWatchClient, s3 client.S3Client) *Commands {
	return &Commands{
		cw: cw,
		s3: s3,
	}
}

func (uc *Commands) CloudWatchGet(args map[string]any) error {
	logs, _, err := uc.cw.GetLogEvents(args["log-stream-name"].(string), args["log-group-name"].(string))
	if err != nil {
		return err
	}
	f, err := os.Create(args["output-path"].(string))
	if err != nil {
		return err
	}
	defer f.Close()
	for _, log := range logs {
		_, err := f.WriteString(log + "\n")
		if err != nil {
			return err
		}
	}
	return nil
}

func (uc *Commands) CloudWatchGetAll(args map[string]any) error {
	start, err := time.Parse("2006-01-02T15:04:05", args["start-time"].(string))
	if err != nil {
		return err
	}
	end, err := time.Parse("2006-01-02T15:04:05", args["end-time"].(string))
	if err != nil {
		return err
	}

	streams, err := uc.cw.GetStreams(args["log-group-name"].(string), start, end)
	if err != nil {
		return err
	}

	if err := os.MkdirAll(args["output-dir"].(string), 0755); err != nil {
		return err
	}

	for _, stream := range streams {
		uc.CloudWatchGet(map[string]any{
			"log-group-name":  args["log-group-name"].(string),
			"log-stream-name": stream.Name,
			"output-path":     filepath.Join(args["output-dir"].(string), fmt.Sprintf("%s.log", stream.Name)),
		})
	}
	return nil
}

func (uc *Commands) CloudWatchListStreams(args map[string]any) error {
	start, err := time.Parse("2006-01-02T15:04:05", args["start-time"].(string))
	if err != nil {
		return err
	}
	end, err := time.Parse("2006-01-02T15:04:05", args["end-time"].(string))
	if err != nil {
		return err
	}
	streams, err := uc.cw.GetStreams(args["log-group-name"].(string), start, end)
	if err != nil {
		return err
	}
	f, err := os.Create(args["output-path"].(string))
	if err != nil {
		return err
	}
	defer f.Close()
	for _, stream := range streams {
		_, err := f.WriteString(stream.Name + "\n")
		if err != nil {
			return err
		}
	}
	return nil
}

func (uc *Commands) S3Download(args map[string]any) error {
	return uc.s3.DownloadFolder(
		args["bucket"].(string),
		args["prefix"].(string),
		args["local-path"].(string),
		args["rewrite"].(bool),
	)
}
