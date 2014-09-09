
.DEFAULT_GOAL := default

AR       = $(TOOLCHAIN_PATH)$(ARCH)ar
AS       = $(TOOLCHAIN_PATH)$(ARCH)as
CC       = $(TOOLCHAIN_PATH)$(ARCH)gcc
CPP      = $(TOOLCHAIN_PATH)$(ARCH)cpp
CXX      = $(TOOLCHAIN_PATH)$(ARCH)g++
LD       = $(TOOLCHAIN_PATH)$(ARCH)ld
NM       = $(TOOLCHAIN_PATH)$(ARCH)nm
RANLIB   = $(TOOLCHAIN_PATH)$(ARCH)ranlib
OBJCOPY  = $(TOOLCHAIN_PATH)$(ARCH)objcopy
OBJDUMP  = $(TOOLCHAIN_PATH)$(ARCH)objdump
LDCONFIG = $(TOOLCHAIN_PATH)$(ARCH)ldconfig

BUILDROOT ?= ../../os/build/buildroot-2014.08/
CHECKSYMS ?= ../checksyms/checksyms.py
CHECKSYMS_LIBC_WHITELIST ?= ../checksyms/libc_whitelist
CHECKSYMS_LIBC ?= $(BUILDROOT)/output/target/lib/libc.so.0
SKLITE_SEPOLICY ?= ../sklite/.cabal-sandbox/bin/sklite-sepolicy

SKLITE_CONFIG=demo.xml
SKLITE = ../sklite/.cabal-sandbox/bin
SKLITE_DRIVERS = $(SKLITE)/sklite-drivers
SKLITE_BOOT = $(SKLITE)/sklite-boot

default:
	@echo "Targets:"
	@echo
	@echo "  host            Compile using the host toolchain (does NOT generate"
	@echo "                  an initramfs)"
	@echo "  cross           Compile using the buildroot cross compiler toolchain"
	@echo "                  (builds an initramfs in $(BASE_INITRAMFS_FILE))"
	@echo "  clean           Clean up"

BUILDROOT_TREE=$(BUILDROOT)/output/target
GENEXT2FS=$(BUILDROOT)/output/host/usr/bin/genext2fs
FAKEROOT=$(BUILDROOT)/output/host/usr/bin/fakeroot
BASE_INITRAMFS_FILE=initramfs-buildroot

# Given a source directory and a target image filename, mount the image and
# install the source directory contents into the image.  Unmount the image when
# finished.
mkimage = $(FAKEROOT) -- $(GENEXT2FS) -d $(1) -N 10000 -b 250000 $(2)

# Build a demo filesystem image by merging the demo programs and
# scripts into the base filesystem image.
initrd: demodir
	cp -pR $(BUILDROOT_TREE)/* $(INITRAMFS_DIR)
	find $(INITRAMFS_DIR)/dev -type f | xargs rm
	$(call mkimage,$(INITRAMFS_DIR),$(shell pwd)/$(BASE_INITRAMFS_FILE))
	rm -rf $(INITRAMFS_DIR)

cross: INITRAMFS_DIR := $(shell mktemp -d)
cross: TOOLCHAIN_PATH = $(BUILDROOT)/output/host/usr/bin/
cross: ARCH = x86_64-buildroot-linux-uclibc-
cross: drivers cells initrd

host: INITRAMFS_DIR=./fs
host: TOOLCHAIN_PATH=
host: drivers cells demodir
