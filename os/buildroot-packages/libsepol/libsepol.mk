LIBSEPOL_VERSION = 2.1.4
LIBSEPOL_SOURCE = libsepol-2.1.4.tar.gz
LIBSEPOL_SITE = http://userspace.selinuxproject.org/releases/20120216/
LIBSEPOL_INSTALL_STAGING = YES
#LIBSEPOL_FLAGS=LDFLAGS="-L$(STAGING_DIR)/lib -L$(STAGING_DIR)/usr/lib -I$(STAGING_DIR)/usr/include"

define LIBSEPOL_BUILD_CMDS
  $(MAKE) $(TARGET_CONFIGURE_OPTS) -C $(@D) DESTDIR=$(TARGET_DIR)
endef

define LIBSEPOL_INSTALL_STAGING_CMDS
  $(MAKE) $(TARGET_CONFIGURE_OPTS) -C $(@D) DESTDIR=$(STAGING_DIR) install
endef

define LIBSEPOL_INSTALL_TARGET_CMDS
  $(MAKE) $(TARGET_CONFIGURE_OPTS) -C $(@D) DESTDIR=$(TARGET_DIR) install
endef

define LIBSEPOL_INSTALL_TARGET_CMDS
  $(MAKE) $(TARGET_CONFIGURE_OPTS) -C $(@D) DESTDIR=$(TARGET_DIR) install
endef

define HOST_LIBSEPOL_BUILD_CMDS
  $(HOST_MAKE_ENV) $(MAKE) LDFLAGS="$(HOST_LDFLAGS)" CFLAGS="$(HOST_CFLAGS)" -C $(@D)
endef

define HOST_LIBSEPOL_INSTALL_CMDS
  $(HOST_MAKE_ENV) $(MAKE) -C $(@D) DESTDIR=$(HOST_DIR) SHLIBDIR=$(HOST_DIR)/usr/lib install
  ln -fs libsepol.so.1 $(HOST_DIR)/usr/lib/libsepol.so
endef

$(eval $(generic-package))
$(eval $(host-generic-package))
