
LIBSEMANAGE_VERSION = 2.1.10
LIBSEMANAGE_SOURCE = libsemanage-2.1.10.tar.gz
LIBSEMANAGE_SITE = http://userspace.selinuxproject.org/releases/20130423/
LIBSEMANAGE_INSTALL_STAGING = YES
LIBSEMANAGE_MAKE_OPT = CFLAGS+="-U_FILE_OFFSET_BITS"
HOST_LIBSEMANAGE_MAKE_OPT = CFLAGS+="-U_FILE_OFFSET_BITS" 
LIBSEMANAGE_DEPENDENCIES = ustr python bzip2 libselinux libsepol

define LIBSEMANAGE_BUILD_CMDS
  $(TARGET_MAKE_ENV) $(MAKE) CC="$(TARGET_CC)" CFLAGS="$(TARGET_CFLAGS)" PREFIX=$(TARGET_DIR)/usr DESTDIR=$(TARGET_DIR)/usr -C $(@D)
  $(TARGET_MAKE_ENV) $(MAKE) CC="$(TARGET_CC)" CFLAGS="$(TARGET_CFLAGS)" PREFIX=$(TARGET_DIR)/usr DESTDIR=$(TARGET_DIR)/usr -C $(@D) swigify
  $(TARGET_MAKE_ENV) $(MAKE) CC="$(TARGET_CC)" CFLAGS="$(TARGET_CFLAGS)" PREFIX=$(TARGET_DIR)/usr DESTDIR=$(TARGET_DIR)/usr -C $(@D) pywrap
endef

define LIBSEMANAGE_INSTALL_STAGING_CMDS
  $(MAKE) $(TARGET_CONFIGURE_OPTS) $(LIBSEMANAGE_MAKE_OPT) \
	PREFIX=$(TARGET_DIR)/usr DESTDIR=$(STAGING_DIR) -C $(@D) install install-pywrap
endef

define LIBSEMANAGE_INSTALL_TARGET_CMDS
  $(MAKE) $(TARGET_CONFIGURE_OPTS) $(LIBSEMANAGE_MAKE_OPT) \
	PREFIX=$(TARGET_DIR)/usr DESTDIR=$(TARGET_DIR) -C $(@D) install install-pywrap
  $(HOST_DIR)/usr/bin/python -m compileall $(TARGET_DIR)/usr/lib/python2.7/site-packages
endef

define HOST_LIBSEMANAGE_BUILD_CMDS
  $(HOST_MAKE_ENV) $(MAKE) $(HOST_LIBSEMANAGE_MAKE_OPT) \
        CFLAGS="$(HOST_CFLAGS)" \
        LDFLAGS="$(HOST_LDFLAGS)" \
	PREFIX=$(HOST_DIR)/usr DESTDIR=$(HOST_DIR)/usr -C $(@D)
endef

define HOST_LIBSEMANAGE_INSTALL_CMDS
  $(HOST_MAKE_ENV) $(MAKE) $(HOST_LIBSEMANAGE_MAKE_OPT) \
	PREFIX=$(HOST_DIR)/usr DESTDIR=$(HOST_DIR) -C $(@D) install
endef

$(eval $(generic-package))
$(eval $(host-generic-package))
