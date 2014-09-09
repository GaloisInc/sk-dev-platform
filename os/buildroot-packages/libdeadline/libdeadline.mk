#############################################################
#
# libdeadline
#
#############################################################

LIBDEADLINE_SITE = ../libdl
LIBDEADLINE_SITE_METHOD = local
LIBDEADLINE_INSTALL_STAGING = YES

define LIBDEADLINE_BUILD_CMDS
  $(MAKE) $(TARGET_CONFIGURE_OPTS) -C $(@D)
endef

define LIBDEADLINE_INSTALL_STAGING_CMDS
  $(MAKE) $(TARGET_CONFIGURE_OPTS) -C $(@D) DESTPREFIX=$(STAGING_DIR) DESTDIR=usr/lib install
endef

define LIBDEADLINE_INSTALL_TARGET_CMDS
  $(MAKE) $(TARGET_CONFIGURE_OPTS) -C $(@D) DESTPREFIX=$(TARGET_DIR) DESTDIR=usr/lib install
endef

$(eval $(generic-package))
$(eval $(host-generic-package))
