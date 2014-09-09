
include ../include.mk

CFLAGS=-Wall -I../libsk
LDFLAGS=-L../libsk -lsk -lpthread

.PHONY: drivers
drivers:
	$(SKLITE_DRIVERS) $(SKLITE_CONFIG) .

cells: pep net1 net2

# Generate C stub code for each cell.
pep_driver.c net1_driver.c net2_driver.c: drivers

pep: pep.c pep_driver.o
	$(CC) $(CFLAGS) -o $@ pep.c pep_driver.o $(LDFLAGS)

net1: net1.c net1_driver.o
	$(CC) $(CFLAGS) -o $@ net1.c net1_driver.o $(LDFLAGS)

net2: net2.c net2_driver.o
	$(CC) $(CFLAGS) -o $@ net2.c net2_driver.o $(LDFLAGS)

scripts:
	$(SKLITE_BOOT) $(SKLITE_CONFIG) .

# Run the 'checksyms' tool on all of the cell binaries.
checksyms:
	python $(CHECKSYMS) $(CHECKSYMS_LIBC) $(CHECKSYMS_LIBC_WHITELIST) ./pep
	python $(CHECKSYMS) $(CHECKSYMS_LIBC) $(CHECKSYMS_LIBC_WHITELIST) ./net1
	python $(CHECKSYMS) $(CHECKSYMS_LIBC) $(CHECKSYMS_LIBC_WHITELIST) ./net2

# Create a directory containing everything that needs to be installed
# into the filesystem image.
demodir: checksyms
	mkdir -p $(INITRAMFS_DIR)/sk
	cp -f pep net1 net2 $(INITRAMFS_DIR)/sk
	$(SKLITE_BOOT) $(SKLITE_CONFIG) $(INITRAMFS_DIR)/sk

	mkdir -p $(INITRAMFS_DIR)/etc/selinux/extension
	$(SKLITE_SEPOLICY) $(SKLITE_CONFIG) $(INITRAMFS_DIR)/etc/selinux/extension

clean:
	rm -f *_driver.[ch]
	rm -f pep net1 net2 *.o *~
	rm -f $(BASE_INITRAMFS_FILE)
	rm -rf fs
