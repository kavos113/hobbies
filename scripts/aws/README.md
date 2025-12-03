# aws scripts

## usage

### build
```
go build -o awsu main.go
```

### commands

required to set AWS credentials

- cloudwatch get <log-group-name> <log-stream-name>
- cloudwatch get_all <log-group-name> <start-time> <end-time>  
    - time: `2006-01-02T15:04:05` format
- cloudwatch list_streams <log-group-name> <start-time> <end-time>  
    - time: `2006-01-02T15:04:05` format
- s3 download <bucket-name> <dir_name> <local_path>