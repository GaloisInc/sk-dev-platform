
LIBCAP_NG_SITE = "http://people.redhat.com/sgrubb/libcap-ng/"
LIBCAP_NG_SOURCE = libcap-ng-0.6.6.tar.gz
LIBCAP_NG_VERSION = 0.6.6
LIBCAP_NG_INSTALL_STAGING = YES

# LIBCAP_NG_CONF_OPT = --enable-tools --disable-pam
# LIBCAP_NG_MAKE_OPT = CFLAGS+="-U_FILE_OFFSET_BITS"
LIBCAP_NG_AUTORECONF = YES

$(eval $(call AUTOTARGETS))
$(eval $(call AUTOTARGETS,host))

