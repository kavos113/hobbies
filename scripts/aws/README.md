# aws scripts

## usage

### build
```
go build -o awsu main.go
```

### commands

required to set AWS credentials

- `cloudwatch get <log-group-name> <log-stream-name> <output-path>`
- `cloudwatch get_all <log-group-name> <start-time> <end-time> <output-dir>`
    - time: `2006-01-02T15:04:05` format
- `cloudwatch list_streams <log-group-name> <start-time> <end-time> <output-path>`
    - time: `2006-01-02T15:04:05` format
- `s3 download <bucket> <prefix> <local-path> --rewrite`  
    - rewrite: optional flag to rewrite existing files