#############################################################
#
# Build the ext3 root filesystem image
#
#############################################################

EXT3_OPTS :=

# ROOTFS_EXT3_DEPENDENCIES = host-genext2fs

define ROOTFS_EXT3_CMD
	PATH=$(TARGET_PATH) fs/ext3/genext3fs.sh -d $(TARGET_DIR) $$@
endef

$(eval $(call ROOTFS_TARGET,ext3))
