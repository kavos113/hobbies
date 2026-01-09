SRCS=$(wildcard src/*.c)
OBJS=$(SRCS:.c=.o)

CC=gcc
CFLAGS=-std=c11 -g -static

ass: $(OBJS)
	$(CC) -o ass $(OBJS) $(LDFLAGS)

%.o: %.c %.h
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f ass src/*.o *~

.PHONY: test clean