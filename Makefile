SUBDIRS := low-level/assembler/riscv \
		   low-level/compiler/c-compiler \
		   low-level/compiler/dentaku \
		   low-level/os/my-mikanos/kernel \
		   reverse/formats/pe

.PHONY: all clean $(SUBDIRS)

all: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

clean:
	for dir in $(SUBDIRS); do \
		$(MAKE) -C $$dir clean; \
	done
		   