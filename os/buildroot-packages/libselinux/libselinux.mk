
LIBSELINUX_VERSION = 2.1.13
LIBSELINUX_SOURCE = libselinux-2.1.13.tar.gz
LIBSELINUX_SITE = http://userspace.selinuxproject.org/releases/20130423/
LIBSELINUX_INSTALL_STAGING = YES
LIBSELINUX_DEPENDENCIES = libsepol python pcre
HOST_LIBSELINUX_DEPENDENCIES = host-libsepol host-python pcre

define LIBSELINUX_BUILD_CMDS
  install package/libselinux/label_file.c $(@D)/src/label_file.c; \
  install package/libselinux/label_db.c $(@D)/src/label_db.c; \
  install package/libselinux/label_x.c $(@D)/src/label_x.c; \
  install package/libselinux/label_media.c $(@D)/src/label_media.c; \
  $(MAKE) $(TARGET_CONFIGURE_OPTS) -C $(@D) LDFLAGS="-lpthread -lpcre" DESTDIR=$(TARGET_DIR)
  $(MAKE) $(TARGET_CONFIGURE_OPTS) -C $(@D) DESTDIR=$(TARGET_DIR) swigify
  $(MAKE) $(TARGET_CONFIGURE_OPTS) -C $(@D) DESTDIR=$(TARGET_DIR) pywrap
endef

define LIBSELINUX_INSTALL_STAGING_CMDS
  $(MAKE) $(TARGET_CONFIGURE_OPTS) -C $(@D) DESTDIR=$(STAGING_DIR) install install-pywrap
endef

define LIBSELINUX_INSTALL_TARGET_CMDS
  $(MAKE) $(TARGET_CONFIGURE_OPTS) -C $(@D) DESTDIR=$(TARGET_DIR) install install-pywrap
  $(HOST_DIR)/usr/bin/python -m compileall $(TARGET_DIR)/usr/lib/python2.7/site-packages
endef

define HOST_LIBSELINUX_BUILD_CMDS
  install package/libselinux/label_file.c $(@D)/src/label_file.c; \
  install package/libselinux/label_db.c $(@D)/src/label_db.c; \
  install package/libselinux/label_x.c $(@D)/src/label_x.c; \
  install package/libselinux/label_media.c $(@D)/src/label_media.c; \
  install package/libselinux/sefcontext_compile.c $(@D)/utils/sefcontext_compile.c; \
  $(HOST_MAKE_ENV) $(MAKE) DESTDIR=$(HOST_DIR) LDFLAGS="$(HOST_LDFLAGS) -lpcre -lpthread" CFLAGS="$(HOST_CFLAGS)" -C $(@D)
endef

define HOST_LIBSELINUX_INSTALL_CMDS
  $(HOST_MAKE_ENV) $(MAKE) -C $(@D) SHLIBDIR=$(HOST_DIR)/usr/lib DESTDIR=$(HOST_DIR) install
  ln -fs libselinux.so.1 $(HOST_DIR)/usr/lib/libselinux.so
endef

$(eval $(generic-package))
$(eval $(host-generic-package))
