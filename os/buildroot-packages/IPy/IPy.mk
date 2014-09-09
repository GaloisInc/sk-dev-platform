
IPY_VERSION = 0.75
IPY_PKG = IPy-0.75.tar.gz
IPY_SITE = http://pypi.python.org/packages/source/I/IPy/
IPY_DEPENDENCIES = python

define IPY_BUILD_CMDS
	(cd $(@D); CC="$(TARGET_CC)" CFLAGS="$(TARGET_CFLAGS)" \
		LDSHARED="$(TARGET_CROSS)gcc -shared" \
		CROSS_COMPILING=yes \
		_python_sysroot=$(STAGING_DIR) \
		_python_srcdir=$(BUILD_DIR)/python$(PYTHON_VERSION) \
		_python_prefix=/usr \
		_python_exec_prefix=/usr \
		$(HOST_DIR)/usr/bin/python setup.py build)
endef

define IPY_INSTALL_TARGET_CMDS
	(cd $(@D); $(HOST_DIR)/usr/bin/python setup.py install \
		--prefix=$(TARGET_DIR)/usr)
endef

$(eval $(generic-package))
$(eval $(host-generic-package))
