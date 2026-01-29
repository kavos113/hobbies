FROM alpine:latest
RUN apk add --no-cache gcc musl-dev
WORKDIR /app
COPY main.c .
RUN gcc -o main main.c
CMD ["./main"]