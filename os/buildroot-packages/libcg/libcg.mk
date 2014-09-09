#############################################################
#
# libcg
#
#############################################################

LIBCG_SITE = "http://kent.dl.sourceforge.net/project/libcg/libcgroup/v.038/"
LIBCG_SOURCE = libcgroup-0.38.tar.bz2
LIBCG_VERSION = 0.38
LIBCG_INSTALL_STAGING = YES

LIBCG_CONF_OPT = --enable-tools --disable-pam CFLAGS="-U_FILE_OFFSET_BITS"
HOST_LIBCG_CONF_OPT = --enable-tools --disable-pam CFLAGS="-U_FILE_OFFSET_BITS"
LIBCG_MAKE_OPT = CFLAGS+="-U_FILE_OFFSET_BITS"
HOST_LIBCG_MAKE_OPT = CFLAGS+="-U_FILE_OFFSET_BITS"
LIBCG_AUTORECONF = YES
HOST_LIBCG_AUTORECONF = YES

$(eval $(generic-package))
$(eval $(host-generic-package))
