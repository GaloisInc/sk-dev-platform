
HERE=$(shell pwd)

KERNEL_DIR=$(HERE)/build/linux-deadline
BUILDROOT_DIR=$(HERE)/build/buildroot-2014.08
REFPOLICY_DIR=$(HERE)/build/refpolicy

KERNEL_IMAGE=$(HERE)/vmlinuz-deadline
BUILDROOT_IMAGE=$(HERE)/initramfs-buildroot

SELINUX_POLICY_DIR=$(BUILDROOT_DIR)/output/target/etc/selinux
SELINUX_POLICY_PREFIX=$(BUILDROOT_DIR)/output/target/usr

.PHONY: check kernel buildroot busybox-wrappers

# Note that refpolicy MUST be built before buildroot, since that way
# the refpolicy files will be picked up by buildroot's filesystem
# image creation process.
all: kernel refpolicy buildroot
	@echo
	@echo
	@echo ========================================================
	@echo "   Kernel image ready at: $(KERNEL_IMAGE)"
	@echo "Buildroot image ready at: $(BUILDROOT_IMAGE)"

check:
	@[ -d $(KERNEL_DIR) ] || { \
	  echo "$(KERNEL_DIR) does not exist; did you run bootstrap.sh?"; \
	  exit 1; }
	@[ -d $(BUILDROOT_DIR) ] || { \
	  echo "$(BUILDROOT_DIR) does not exist; did you run bootstrap.sh?"; \
	  exit 1; }
	@[ -d $(REFPOLICY_DIR) ] || { \
	  echo "$(REFPOLICY_DIR) does not exist; did you run bootstrap.sh?"; \
	  exit 1; }

refpolicy: check
	install $(HERE)/configs/refpolicy.users $(REFPOLICY_DIR)/policy/users
	install -d $(BUILDROOT_DIR)/overlay/etc/selinux
	install $(HERE)/configs/selinux/config $(BUILDROOT_DIR)/overlay/etc/selinux/config
	install -d $(BUILDROOT_DIR)/overlay/etc/init.d
	install -m 0755 $(HERE)/configs/startup_script $(BUILDROOT_DIR)/overlay/etc/init.d/S00selinux
	install -m 0755 $(HERE)/configs/rcS $(BUILDROOT_DIR)/overlay/etc/init.d/rcS

kernel: check
	$(MAKE) -j4 -C $(KERNEL_DIR) bzImage
	cp $(KERNEL_DIR)/arch/x86/boot/bzImage $(KERNEL_IMAGE)

buildroot: check
	mkdir -p $(BUILDROOT_DIR)/output/target/etc/init.d
	install -m 0755 $(HERE)/configs/S99sk $(BUILDROOT_DIR)/output/target/etc/init.d/S99sk
	$(MAKE) -C $(BUILDROOT_DIR)
	cp $(BUILDROOT_DIR)/output/images/rootfs.cpio $(BUILDROOT_IMAGE)

busybox-wrappers:
	CC=build/buildroot-2014.08/output/host/usr/bin/x86_64-buildroot-linux-uclibc-gcc runghc utils/BusyboxWrapper.hs build/buildroot-2014.08/output/target/bin build/buildroot-2014.08/output/target/sbin build/buildroot-2014.08/output/target/usr/bin build/buildroot-2014.08/output/target/usr/sbin

clean:
	rm $(KERNEL_IMAGE) $(BUILDROOT_IMAGE)
