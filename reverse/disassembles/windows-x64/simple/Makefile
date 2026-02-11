CC=gcc
CFLAGS=-std=c11

all: asm

main: main.o
	$(CC) -o $@ $^ $(CFLAGS)

asm: main
	objdump -d -M intel main.exe > main.asm

clean:
	rm -f main main.o