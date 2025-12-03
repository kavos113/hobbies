package client

import (
	"context"
	"fmt"
	"io"
	"os"
	"path/filepath"

	"github.com/aws/aws-sdk-go-v2/aws"
	"github.com/aws/aws-sdk-go-v2/service/s3"
)

type S3Client struct {
	client *s3.Client
}

func NewS3Client(cfg aws.Config) *S3Client {
	return &S3Client{
		client: s3.NewFromConfig(cfg),
	}
}

func (c *S3Client) DownloadFolder(bucket, prefix, localPath string, rewrite bool) error {
	ctx := context.Background()

	if err := os.MkdirAll(localPath, 0755); err != nil {
		return err
	}

	paginator := s3.NewListObjectsV2Paginator(c.client, &s3.ListObjectsV2Input{
		Bucket: &bucket,
		Prefix: &prefix,
	})

	for paginator.HasMorePages() {
		page, err := paginator.NextPage(ctx)
		if err != nil {
			return err
		}

		for _, obj := range page.Contents {
			filename := filepath.Base(*obj.Key)
			localFilePath, err := filepath.Abs(filepath.Join(localPath, filename))
			if err != nil {
				return err
			}

			if !rewrite {
				if _, err := os.Stat(localFilePath); err == nil {
					continue
				}
			}

			getObjInput := &s3.GetObjectInput{
				Bucket: &bucket,
				Key:    obj.Key,
			}
			resp, err := c.client.GetObject(ctx, getObjInput)
			if err != nil {
				return err
			}
			defer resp.Body.Close()

			localFile, err := os.Create(localFilePath)
			if err != nil {
				return err
			}
			defer localFile.Close()

			_, err = io.Copy(localFile, resp.Body)
			if err != nil {
				return err
			}

			fmt.Printf("Downloaded: %s\n", localFilePath)
		}
	}

	return nil
}
