AS = nasm
ASFLAGS = -f win64
LD = link
LIBS = kernel32.lib user32.lib
LDFLAGS = /subsystem:console /entry:main

TARGET = hello.exe
OBJS = hello.obj

all: $(TARGET)

$(TARGET): $(OBJS)
	$(LD) $(LDFLAGS) /out:$@ $^ $(LIBS)

%.obj: %.asm
	$(AS) $(ASFLAGS) -o $@ $<

clean:
	del $(OBJS) $(TARGET)

.PHONY: all clean