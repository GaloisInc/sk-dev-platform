
SEPOLGEN_VERSION = 1.1.9
SEPOLGEN_PKG = sepolgen-1.1.9.tar.gz
SEPOLGEN_SITE = http://userspace.selinuxproject.org/releases/20130423/
SEPOLGEN_INSTALL_STAGING = YES
SEPOLGEN_DEPENDENCIES = flex

define SEPOLGEN_BUILD_CMDS
  $(MAKE) $(TARGET_CONFIGURE_OPTS) -C $(@D) DESTDIR=$(TARGET_DIR) PYTHONLIBDIR=/usr/lib/python2.7/site-packages 
endef

define SEPOLGEN_INSTALL_STAGING_CMDS
  $(MAKE) $(TARGET_CONFIGURE_OPTS) -C $(@D) DESTDIR=$(STAGING_DIR) PYTHONLIBDIR=/usr/lib/python2.7/site-packages install
  $(HOST_DIR)/usr/bin/python -m compileall $(TARGET_DIR)/usr/lib/python2.7/site-packages
endef

define SEPOLGEN_INSTALL_TARGET_CMDS
  $(MAKE) $(TARGET_CONFIGURE_OPTS) -C $(@D) DESTDIR=$(TARGET_DIR) PYTHONLIBDIR=/usr/lib/python2.7/site-packages install
  $(HOST_DIR)/usr/bin/python -m compileall $(TARGET_DIR)/usr/lib/python2.7/site-packages
endef

define HOST_SEPOLGEN_BUILD_CMDS
  $(HOST_MAKE_ENV) $(MAKE) $(HOST_CONFIGURE_OPTS) -C $(@D) DESTDIR=$(HOST_DIR) PYTHONLIBDIR=$(HOST_DIR)/usr/lib/python2.7/site-packages 
endef

define HOST_SEPOLGEN_INSTALL_CMDS
  $(HOST_MAKE_ENV) $(MAKE) -C $(@D) DESTDIR=$(HOST_DIR) PYTHONLIBDIR=$(HOST_DIR)/usr/lib/python2.7/site-packages install
endef

$(eval $(generic-package))
$(eval $(host-generic-package))
