SRCS=$(wildcard src/*.c)
OBJS=$(SRCS:.c=.o)

CC=gcc
CFLAGS=-std=c11 -g -static

all: acc

acc: $(OBJS)
	$(CC) -o acc $(OBJS) $(LDFLAGS)

%.o: %.c %.h
	$(CC) $(CFLAGS) -c $< -o $@

test: acc
	./test.sh

clean:
	rm -f acc *.o *~ tmp*

.PHONY: test clean