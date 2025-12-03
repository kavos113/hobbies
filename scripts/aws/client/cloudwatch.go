package client

import (
	"context"
	"fmt"
	"time"

	"github.com/aws/aws-sdk-go-v2/aws"
	"github.com/aws/aws-sdk-go-v2/service/cloudwatchlogs"
	"github.com/aws/aws-sdk-go-v2/service/cloudwatchlogs/types"
)

var (
	startFromHead = true
	descending    = true
)

type CloudWatchClient struct {
	client *cloudwatchlogs.Client
}

func NewCloudWatchClient(cfg aws.Config) *CloudWatchClient {
	return &CloudWatchClient{
		client: cloudwatchlogs.NewFromConfig(cfg),
	}
}

type LogStream struct {
	Name string
	Time time.Time
}

func (c *CloudWatchClient) GetStreams(logGroup string, start, end time.Time) ([]LogStream, error) {
	input := &cloudwatchlogs.DescribeLogStreamsInput{
		LogGroupName: &logGroup,
		OrderBy:      types.OrderByLastEventTime,
		Descending:   &descending,
	}

	var streams []LogStream
	for {
		output, err := c.client.DescribeLogStreams(context.Background(), input)
		if err != nil {
			return nil, err
		}

		for _, stream := range output.LogStreams {
			if stream.LastEventTimestamp != nil {
				continue
			}
			streamTime := time.UnixMilli(*stream.LastEventTimestamp)

			if streamTime.After(end) {
				continue
			}

			if streamTime.Before(start) {
				return streams, nil
			}

			streams = append(streams, LogStream{
				Name: *stream.LogStreamName,
				Time: streamTime,
			})
		}

		if output.NextToken == nil {
			break
		}

		input.NextToken = output.NextToken
	}

	return streams, nil
}

func (c *CloudWatchClient) GetLogEvents(logStream, logGroup string) ([]string, int, error) {
	input := &cloudwatchlogs.GetLogEventsInput{
		LogGroupName:  &logGroup,
		LogStreamName: &logStream,
		StartFromHead: &startFromHead,
	}

	var events []string
	for {
		output, err := c.client.GetLogEvents(context.Background(), input)
		if err != nil {
			return nil, 0, err
		}

		for _, event := range output.Events {
			events = append(events, *event.Message)
		}

		fmt.Printf("\r%d downloaded", len(events))

		if input.NextToken != nil && *input.NextToken == *output.NextForwardToken {
			break
		}

		input.NextToken = output.NextForwardToken

		if input.NextToken == nil {
			break
		}
	}

	return events, len(events), nil
}
