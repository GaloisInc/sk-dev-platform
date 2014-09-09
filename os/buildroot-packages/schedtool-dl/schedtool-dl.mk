#############################################################
#
# schedtool-dl
#
#############################################################

SCHEDTOOL_DL_SITE = ../schedtool-dl
SCHEDTOOL_DL_SITE_METHOD = local
SCHEDTOOL_DL_INSTALL_STAGING = YES

define SCHEDTOOL_DL_BUILD_CMDS
  $(MAKE) $(TARGET_CONFIGURE_OPTS) -C $(@D) LDFLAGS="-lm"
endef

define SCHEDTOOL_DL_INSTALL_STAGING_CMDS
  $(MAKE) $(TARGET_CONFIGURE_OPTS) -C $(@D) DESTPREFIX=$(STAGING_DIR) install
endef

define SCHEDTOOL_DL_INSTALL_TARGET_CMDS
  $(MAKE) $(TARGET_CONFIGURE_OPTS) -C $(@D) DESTPREFIX=$(TARGET_DIR) install
endef

$(eval $(generic-package))
