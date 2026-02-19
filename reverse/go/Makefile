all:
	go build -ldflags="-s -w" -o main.exe main.go

elf:
	$env:GOOS="linux"; $env:GOARCH="amd64"; go build -ldflags="-s -w" -o main main.go