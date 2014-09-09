
USTR_SITE = http://www.and.org/ustr/ustr.git
USTR_SITE_METHOD = git
USTR_VERSION = master

define USTR_BUILD_CMDS
  install package/ustr/Makefile $(@D)/Makefile; \
  $(MAKE) $(TARGET_CONFIGURE_OPTS) $(USTR_MAKE_OPT) \
	HIDE= prefix=$(TARGET_DIR)/usr -C $(@D)
#  LD_LIBRARY_PATH=$(TARGET_DIR)/lib $(MAKE) $(TARGET_CONFIGURE_OPTS) $(USTR_MAKE_OPT) \
#	HIDE= prefix=$(TARGET_DIR)/usr -C $(@D)
endef

define USTR_INSTALL_STAGING_CMDS
  $(MAKE) $(TARGET_CONFIGURE_OPTS) $(USTR_MAKE_OPT) -C $(@D) prefix=$(TARGET_DIR)/usr install
endef

define USTR_INSTALL_TARGET_CMDS
  $(MAKE) $(TARGET_CONFIGURE_OPTS) $(USTR_MAKE_OPT) -C $(@D) prefix=$(TARGET_DIR)/usr install
endef

define HOST_USTR_BUILD_CMDS
  install package/ustr/Makefile $(@D)/Makefile; \
  LD_LIBRARY_PATH=$(HOST_DIR)/lib $(MAKE) $(HOST_CONFIGURE_OPTS) $(USTR_MAKE_OPT) \
	HIDE= prefix=$(HOST_DIR)/usr -C $(@D)
endef
define HOST_USTR_INSTALL_CMDS
  $(HOST_MAKE_ENV) $(MAKE) $(HOST_CONFIGURE_OPTS) $(USTR_MAKE_OPT) -C $(@D) prefix=$(HOST_DIR)/usr install
endef

$(eval $(generic-package))
$(eval $(host-generic-package))
