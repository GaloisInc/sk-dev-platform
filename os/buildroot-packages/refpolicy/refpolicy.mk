
REFPOLICY_VERSION = 2.20120725
REFPOLICY_SOURCE = refpolicy-2.20120725.tar.bz2
REFPOLICY_SITE = http://oss.tresys.com/files/refpolicy
REFPOLICY_DEPENDENCIES = host-libsepol host-libsemanage host-policycoreutils host-checkpolicy host-sepolgen

REFPOLICY_TOOL_PATHS = \
  PYTHON=$(HOST_DIR)/usr/bin/python \
  BINDIR=$(HOST_DIR)/usr/bin  \
  SBINDIR=$(HOST_DIR)/usr/sbin \
  M4=$(HOST_DIR)/usr/bin/m4

define REFPOLICY_BUILD_CMDS
  $(MAKE) -C $(@D) $(TARGET_CONFIGURE_OPTS) $(REFPOLICY_TOOL_PATHS) DESTDIR=$(STAGING_DIR) CC=/usr/bin/gcc conf policy
endef

define REFPOLICY_INSTALL_TARGET_CMDS
  $(MAKE) -C $(@D) $(REFPOLICY_TOOL_PATHS) DESTDIR=$(TARGET_DIR) install install-headers
endef

$(eval $(generic-package))
$(eval $(host-generic-package))
