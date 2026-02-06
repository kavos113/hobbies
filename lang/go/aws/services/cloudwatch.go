package services

import (
	"context"
	"fmt"

	"github.com/aws/aws-sdk-go-v2/aws"
	"github.com/aws/aws-sdk-go-v2/service/cloudwatchlogs"
)

var (
	startFromHead = true
)

type CloudWatchClient struct {
	client *cloudwatchlogs.Client
}

func NewCloudWatchClient(cfg aws.Config) *CloudWatchClient {
	return &CloudWatchClient{
		client: cloudwatchlogs.NewFromConfig(cfg),
	}
}

func (c *CloudWatchClient) GetLogEvents(logStream, logGroup string) ([]string, int, error) {
	input := &cloudwatchlogs.GetLogEventsInput{
		LogStreamName: &logStream,
		LogGroupName:  &logGroup,
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

func (c *CloudWatchClient) StartLogQuery(logGroup, queryString string, startTime, endTime int64) (string, error) {
	input := &cloudwatchlogs.StartQueryInput{
		LogGroupName: &logGroup,
		QueryString:  &queryString,
		StartTime:    &startTime,
		EndTime:      &endTime,
	}

	output, err := c.client.StartQuery(context.Background(), input)
	if err != nil {
		return "", err
	}

	return *output.QueryId, nil
}

func (c *CloudWatchClient) GetLogQueryResults(queryID string) (*cloudwatchlogs.GetQueryResultsOutput, error) {
	input := &cloudwatchlogs.GetQueryResultsInput{
		QueryId: &queryID,
	}

	output, err := c.client.GetQueryResults(context.Background(), input)
	if err != nil {
		return nil, err
	}

	return output, nil
}
