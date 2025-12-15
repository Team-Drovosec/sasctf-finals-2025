# RISC-V bare-metal build for the Gatekeeper example program
	
CROSS_COMPILE ?= riscv64-linux-gnu-
CC              := $(CROSS_COMPILE)gcc
AR              := $(CROSS_COMPILE)ar
OBJCOPY         := $(CROSS_COMPILE)objcopy
OBJDUMP         := $(CROSS_COMPILE)objdump

COMPTIME_SEED ?= $(shell date +%s)
$(info Building with COMPTIME_SEED=$(COMPTIME_SEED))

# Inputs from Python:
#  - SRCS: generated source(s), may be absolute paths
#  - BUILD_DIR: output dir
#  - EXE_NAME: basename of output (prog)
#  - EXTRA_CFLAGS: extra CFLAGS to add
SRCS      ?= main.c
BUILD_DIR ?= build
EXE_NAME  ?= example
EXTRA_CFLAGS ?= -O3

# Core sources that are always linked
CORE_SRCS ?= trusted_lib_api.c

ARCH_FLAGS := -march=rv64im -mabi=lp64
CFLAGS     := $(ARCH_FLAGS) -ffreestanding -Wall -Wextra \
			  -DCOMPTIME_SEED=$(COMPTIME_SEED) -I. \
			  $(EXTRA_CFLAGS)
LDFLAGS    := -static -nostdlib -nostartfiles \
		      -T ld_script.ld \
			  -Wl,-Map=$(BUILD_DIR)/$(EXE_NAME).map \
			  -Wl,--build-id=none

BASE_ADDR := 0x300000

# --- Object mapping that works with absolute paths ------------------------

ALL_SRCS := $(SRCS) $(CORE_SRCS)
# Objects live under BUILD_DIR, filename-only (drop directories)
OBJS := $(addprefix $(BUILD_DIR)/,$(notdir $(ALL_SRCS:.c=.o)))

# Per-source compile rules (handles absolute/relative SRCS)
define COMPILE_C
$(BUILD_DIR)/$(notdir $(1:.c=.o)): $(1) | $(BUILD_DIR)
	$$(CC) $$(CFLAGS) -c $$< -o $$@
endef
$(foreach S,$(ALL_SRCS),$(eval $(call COMPILE_C,$(S))))

# -------------------------------------------------------------------------

MINISTD_SRC     := ministd.c
MINISTD_OBJ     := $(BUILD_DIR)/ministd.o
MINISTD_LIB     := $(BUILD_DIR)/libministd.a

.PHONY: all clean distclean disasm encode

all: $(BUILD_DIR)/$(EXE_NAME).bin

$(BUILD_DIR):
	@mkdir -p $@

$(BUILD_DIR)/%.o: %.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

$(MINISTD_OBJ): $(MINISTD_SRC) ministd.h | $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

$(MINISTD_LIB): $(MINISTD_OBJ)
	$(AR) rcs $@ $^

$(BUILD_DIR)/$(EXE_NAME).elf: $(OBJS) $(MINISTD_LIB) ld_script.ld | $(BUILD_DIR)
	$(CC) $(LDFLAGS) $(OBJS) $(MINISTD_LIB) -o $@

$(BUILD_DIR)/$(EXE_NAME).bin: $(BUILD_DIR)/$(EXE_NAME).elf
	$(OBJCOPY) --change-addresses=-$(BASE_ADDR) -O binary $< $@

disasm: $(BUILD_DIR)/$(EXE_NAME).bin
	$(OBJDUMP) -b binary -D $< -m riscv:rv64 --adjust-vma=0x300000

encode: $(BUILD_DIR)/$(EXE_NAME).bin
	base64 -w0 $<

clean:
	rm -rf $(BUILD_DIR)

distclean: clean

