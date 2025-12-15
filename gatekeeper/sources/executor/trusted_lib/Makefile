# Build the trusted library blob for the executor

CROSS_COMPILE ?= riscv64-linux-gnu-
CC              := $(CROSS_COMPILE)gcc
OBJCOPY         := $(CROSS_COMPILE)objcopy
OBJDUMP         := $(CROSS_COMPILE)objdump

BUILD_DIR       := build
TARGET          := trusted_lib

ARCH_FLAGS      := -march=rv64im -mabi=lp64 -fno-omit-frame-pointer -fno-optimize-sibling-calls -static -nostdlib -nostartfiles
CFLAGS          := $(ARCH_FLAGS) -Os -ffreestanding -Wall -Wextra -I.
LDFLAGS         := $(ARCH_FLAGS) -T ld_script.ld \
		   -Wl,-Map="$(BUILD_DIR)/$(TARGET).map" \
		   -Wl,--build-id=none
		   
BASE_ADDR       := 0x80000000

SRCS            := trusted_lib.c
OBJS            := $(addprefix $(BUILD_DIR)/,$(SRCS:.c=.o))

.PHONY: all clean distclean disasm encode

all: $(BUILD_DIR)/$(TARGET).bin

$(BUILD_DIR):
	mkdir -p $@

$(BUILD_DIR)/%.o: %.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/$(TARGET).elf: $(OBJS) trusted_scrub_rv64.S ld_script.ld
	$(CC) $(LDFLAGS) $(OBJS) trusted_scrub_rv64.S -o $@

$(BUILD_DIR)/$(TARGET).bin: $(BUILD_DIR)/$(TARGET).elf
	$(OBJCOPY) --change-addresses=-$(BASE_ADDR) -O binary $< $@

disasm: $(BUILD_DIR)/$(TARGET).bin
	$(OBJDUMP) -b binary -m riscv:rv64 --adjust-vma=$(BASE_ADDR) -D $<

encode: $(BUILD_DIR)/$(TARGET).bin
	base64 -w0 $<

clean:
	rm -rf $(BUILD_DIR)

distclean: clean

