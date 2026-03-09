AS = nasm
ASFLAGS = -f win64 -g -F cv8
LD = link
LIBS = kernel32.lib user32.lib
LDFLAGS = /subsystem:console /entry:main /debug

TARGET = main.exe
OBJS = main.obj

all: $(TARGET)

$(TARGET): $(OBJS)
	$(LD) $(LDFLAGS) /out:$@ $^ $(LIBS)

%.obj: %.asm
	$(AS) $(ASFLAGS) -o $@ $<

clean:
	del $(OBJS) $(TARGET)

.PHONY: all clean