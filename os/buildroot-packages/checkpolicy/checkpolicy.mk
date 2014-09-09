
CHECKPOLICY_VERSION = 2.1.12
CHECKPOLICY_SOURCE = checkpolicy-2.1.12.tar.gz
CHECKPOLICY_SITE = http://userspace.selinuxproject.org/releases/20130423/
HOST_CHECKPOLICY_DEPENDENCIES = host-flex host-bison host-libsepol host-libselinux
CHECKPOLICY_DEPENDENCIES = flex host-flex host-bison libsepol libselinux

define CHECKPOLICY_BUILD_CMDS
  echo "#include <ctype.h>" > "$(@D)/checkpolicy.c.tmp"
  cat "$(@D)/checkpolicy.c" >> "$(@D)/checkpolicy.c.tmp"
  mv "$(@D)/checkpolicy.c.tmp" "$(@D)/checkpolicy.c"
  $(MAKE) CC="$(TARGET_CC)" LD="$(TARGET_LD)" -C $(@D) DESTDIR="$(TARGET_DIR)"
endef

define CHECKPOLICY_INSTALL_TARGET_CMDS
  touch $(@D)/test/dispol.o $(@D)/test/dismod.o
  touch $(@D)/test/dispol $(@D)/test/dismod
  $(MAKE) CC="$(TARGET_CC)" LD="$(TARGET_LD)" -C $(@D) DESTDIR="$(TARGET_DIR)" install
endef

define HOST_CHECKPOLICY_BUILD_CMDS
  $(HOST_MAKE_ENV) $(MAKE) DESTDIR=$(HOST_DIR) LDFLAGS="$(HOST_LDFLAGS)" CFLAGS="$(HOST_CFLAGS)" -C $(@D)
endef

define HOST_CHECKPOLICY_INSTALL_CMDS
  $(HOST_MAKE_ENV) $(MAKE) -C $(@D) DESTDIR=$(HOST_DIR) install
endef

$(eval $(generic-package))
$(eval $(host-generic-package))
