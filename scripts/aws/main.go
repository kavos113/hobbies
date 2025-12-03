package main

import (
	"context"
	"fmt"
	"os"

	"github.com/aws/aws-sdk-go-v2/config"
	"github.com/kavos113/hobbies/scripts/aws/cli"
	"github.com/kavos113/hobbies/scripts/aws/client"
	"github.com/kavos113/hobbies/scripts/aws/usecase"
)

func main() {
	cfg, err := config.LoadDefaultConfig(context.Background())
	if err != nil {
		panic(err)
	}
	cw := client.NewCloudWatchClient(cfg)
	s3 := client.NewS3Client(cfg)

	c := cli.NewCli()
	cu := usecase.NewCommands(*cw, *s3)

	c.AddCommand("cloudwatch")
	c.AddSubCommand(
		"cloudwatch",
		"get",
		"Get log events from a CloudWatch log stream",
		[]cli.CommandOption{
			{Name: "log-group-name", Type: cli.OptionTypeString, IsRequired: true, NoName: true},
			{Name: "log-stream-name", Type: cli.OptionTypeString, IsRequired: true, NoName: true},
			{Name: "output-path", Type: cli.OptionTypeString, IsRequired: true, NoName: true},
		},
		cu.CloudWatchGet,
	)
	c.AddSubCommand(
		"cloudwatch",
		"get_all",
		"Get all log events from a CloudWatch log group",
		[]cli.CommandOption{
			{Name: "log-group-name", Type: cli.OptionTypeString, IsRequired: true, NoName: true},
			{Name: "start-time", Type: cli.OptionTypeString, IsRequired: true, NoName: true},
			{Name: "end-time", Type: cli.OptionTypeString, IsRequired: true, NoName: true},
			{Name: "output-dir", Type: cli.OptionTypeString, IsRequired: true, NoName: true},
		},
		cu.CloudWatchGetAll,
	)
	c.AddSubCommand(
		"cloudwatch",
		"list_streams",
		"List log streams from a CloudWatch log group",
		[]cli.CommandOption{
			{Name: "log-group-name", Type: cli.OptionTypeString, IsRequired: true, NoName: true},
			{Name: "start-time", Type: cli.OptionTypeString, IsRequired: true, NoName: true},
			{Name: "end-time", Type: cli.OptionTypeString, IsRequired: true, NoName: true},
			{Name: "output-path", Type: cli.OptionTypeString, IsRequired: true, NoName: true},
		},
		cu.CloudWatchListStreams,
	)

	c.AddCommand("s3")
	c.AddSubCommand(
		"s3",
		"download",
		"Download a folder from S3",
		[]cli.CommandOption{
			{Name: "bucket", Type: cli.OptionTypeString, IsRequired: true, NoName: true},
			{Name: "prefix", Type: cli.OptionTypeString, IsRequired: true, NoName: true},
			{Name: "local-path", Type: cli.OptionTypeString, IsRequired: true, NoName: true},
			{Name: "rewrite", Type: cli.OptionTypeBool, IsRequired: false},
		},
		cu.S3Download,
	)

	err = c.ParseAndExecute(os.Args)
	if err != nil {
		fmt.Println("Error:", err)
		os.Exit(1)
	}
}
