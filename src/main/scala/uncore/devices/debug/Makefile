# Recursive make is bad, but in this case we're cross compiling which is a
# pretty unusual use case.

CC = $(RISCV)/bin/riscv32-unknown-elf-gcc
OBJCOPY = $(RISCV)/bin/riscv32-unknown-elf-objcopy

COMPILE = $(CC) -nostdlib -nostartfiles -I$(RISCV)/include/ -Tlink.ld

ELFS = debug_rom
DEPS = debug_rom.S link.ld

all: $(patsubst %,%.h,$(ELFS))

%.scala: %.raw
	xxd -i $^ | sed -e "s/^unsigned char debug_rom_raw\[\] = {/def debugRomContents : Array[Byte] = Array (/" \
	-e  "s/};/)/" \
	-e  "s/^unsigned int debug_rom_raw_len/def debugRomLen/"  > $@

%.raw:	%
	$(OBJCOPY) -O binary --only-section .text $^ $@

debug_rom:	$(DEPS)
	$(COMPILE) -o $@ $^

clean:
	rm -f $(ELFS) debug_rom*.raw debug_rom*.h
