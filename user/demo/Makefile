
include ../include.mk

CFLAGS=-Wall -I../libsk
LDFLAGS=-L../libsk -lsk -laio

.PHONY: drivers
drivers:
	$(SKLITE_DRIVERS) $(SKLITE_CONFIG) .

cells: cell1 cell2 disk1 disk2

# Generate C stub code for each cell.
cell1_driver.c cell2_driver.c disk1_driver.c disk2_driver.c: drivers

cell1: cell1.c cell1_driver.o
	$(CC) $(CFLAGS) -o $@ cell1.c cell1_driver.o $(LDFLAGS)

cell2: cell2.c cell2_driver.o
	$(CC) $(CFLAGS) -o $@ cell2.c cell2_driver.o $(LDFLAGS)

disk1: disk1.c disk1_driver.o
	$(CC) $(CFLAGS) -o $@ disk1.c disk1_driver.o $(LDFLAGS)

disk2: disk2.c disk2_driver.o
	$(CC) $(CFLAGS) -o $@ disk2.c disk2_driver.o $(LDFLAGS)

scripts:
	$(SKLITE_BOOT) $(SKLITE_CONFIG) .

# Run the 'checksyms' tool on all of the cell binaries.
checksyms:
	python $(CHECKSYMS) $(CHECKSYMS_LIBC) $(CHECKSYMS_LIBC_WHITELIST) ./cell1
	python $(CHECKSYMS) $(CHECKSYMS_LIBC) $(CHECKSYMS_LIBC_WHITELIST) ./cell2
	python $(CHECKSYMS) $(CHECKSYMS_LIBC) $(CHECKSYMS_LIBC_WHITELIST) ./disk1
	python $(CHECKSYMS) $(CHECKSYMS_LIBC) $(CHECKSYMS_LIBC_WHITELIST) ./disk2

# Create a directory containing everything that needs to be installed
# into the filesystem image.
demodir: checksyms
	mkdir -p $(INITRAMFS_DIR)/sk
	cp -f cell1 cell2 disk1 disk2 $(INITRAMFS_DIR)/sk
	dd if=/dev/zero of=$(INITRAMFS_DIR)/sk/disk1.dat bs=1024 count=1024
	dd if=/dev/zero of=$(INITRAMFS_DIR)/sk/disk2.dat bs=1024 count=1024

	$(SKLITE_BOOT) $(SKLITE_CONFIG) $(INITRAMFS_DIR)/sk
	mkdir -p $(INITRAMFS_DIR)/etc/selinux/extension
	$(SKLITE_SEPOLICY) $(SKLITE_CONFIG) $(INITRAMFS_DIR)/etc/selinux/extension

clean:
	rm -f *_driver.[ch]
	rm -f cell1 cell2 disk1 disk2 *.o *~
	rm -f $(BASE_INITRAMFS_FILE)
	rm -rf fs
