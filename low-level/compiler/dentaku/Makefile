CFLAGS=-std=c11 -g -static

acc: main.c

test: acc
	./test.sh

clean:
	rm -f acc *.o *~ tmp*

.PHONY: test clean