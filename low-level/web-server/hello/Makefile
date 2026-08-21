SRCS=$(wildcard src/*.c)
OBJS=$(SRCS:.c=.o)
TARGET=main

CC=gcc
CFLAGS=-std=c11 -g -static

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJS) $(LDFLAGS)

%.o: %.c %.h
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(TARGET) src/*.o *~ tmp*