
POLICYCOREUTILS_VERSION = 2.1.14
POLICYCOREUTILS_PKG = policycoreutils-2.1.14.tar.gz
POLICYCOREUTILS_SITE = http://userspace.selinuxproject.org/releases/20130423/
POLICYCOREUTILS_INSTALL_STAGING = YES
POLICYCOREUTILS_MAKE_OPT = CPPFLAGS+="-U_FILE_OFFSET_BITS" CFLAGS+="-U_FILE_OFFSET_BITS -I$(TARGET_DIR)/usr/include" LDFLAGS+="-L$(TARGET_DIR)/usr/lib -lintl -Wl,-rpath=$(TARGET_DIR)/usr/lib"
HOST_POLICYCOREUTILS_MAKE_OPT = CFLAGS+="-U_FILE_OFFSET_BITS" LDFLAGS+="-Wl,-rpath=$(HOST_DIR)/usr/lib"
POLICYCOREUTILS_DEPENDENCIES = python libsepol libselinux libcap-ng libsemanage libglib2 libcgroup
# $(if $(BR2_NEEDS_GETTEXT),gettext)

define POLICYCOREUTILS_BUILD_CMDS
  install package/policycoreutils/Makefile.no_restorecond $(@D)/Makefile; \
  install package/policycoreutils/Makefile.restorecond $(@D)/restorecond/Makefile; \
  install package/policycoreutils/newrole.c $(@D)/newrole/newrole.c; \
  $(MAKE) $(TARGET_CONFIGURE_OPTS) $(POLICYCOREUTILS_MAKE_OPT) \
	PREFIX=$(TARGET_DIR)/usr DESTDIR=$(TARGET_DIR)/usr -C $(@D)
endef

define POLICYCOREUTILS_INSTALL_STAGING_CMDS
  $(MAKE) $(TARGET_CONFIGURE_OPTS) $(POLICYCOREUTILS_MAKE_OPT) -C $(@D) DESTDIR=$(STAGING_DIR) install
endef

define POLICYCOREUTILS_INSTALL_TARGET_CMDS
  $(MAKE) $(TARGET_CONFIGURE_OPTS) $(POLICYCOREUTILS_MAKE_OPT) -C $(@D) DESTDIR=$(TARGET_DIR) install
  $(HOST_DIR)/usr/bin/python -m compileall $(TARGET_DIR)/usr/lib/python2.7/site-packages
endef

define HOST_POLICYCOREUTILS_BUILD_CMDS
  install package/policycoreutils/Makefile.no_restorecond $(@D)/Makefile; \
  install package/policycoreutils/Makefile.restorecond $(@D)/restorecond/Makefile; \
  install package/policycoreutils/newrole.c $(@D)/newrole/newrole.c; \
  $(HOST_MAKE_ENV) $(MAKE)  CFLAGS="$(HOST_CFLAGS)" LDFLAGS="$(HOST_LDFLAGS)" PREFIX=$(HOST_DIR)/usr DESTDIR=$(HOST_DIR)/usr -C $(@D)
endef

define HOST_POLICYCOREUTILS_INSTALL_CMDS
  $(HOST_MAKE_ENV) $(MAKE)  -C $(@D) DESTDIR=$(HOST_DIR) install
endef

$(eval $(generic-package))
$(eval $(host-generic-package))
