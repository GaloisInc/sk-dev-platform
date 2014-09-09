#!/bin/bash

set -e

# Bootstrap the separation kernel OS build environment by fetching
# source code repositories.

HERE="$(cd "$(dirname "$0")" ; pwd)"
BUILD=$HERE/os/build

KERNEL_REPO=${KERNEL_REPO:-https://github.com/GaloisInc/linux-deadline.git}
KERNEL_BRANCH=ospert/sched-dl-V3-original

LIBDEADLINE_REPO=${LIBDEADLINE_REPO:-https://github.com/GaloisInc/libdl.git}
LIBDEADLINE_BRANCH=master

SCHEDTOOL_REPO=${SCHEDTOOL_REPO:-https://github.com/GaloisInc/schedtool-dl.git}
SCHEDTOOL_BRANCH=latest/2.6.36-dl-V3

BUILDROOT_RELEASE=2014.08
BUILDROOT_PKG=buildroot-${BUILDROOT_RELEASE}.tar.gz
BUILDROOT_URL=http://buildroot.org/downloads/${BUILDROOT_PKG}

REFPOLICY_DIR=refpolicy-2.20110726
REFPOLICY_PKG=${REFPOLICY_DIR}.tar.bz2
REFPOLICY_URL=http://oss.tresys.com/files/refpolicy/${REFPOLICY_PKG}

# Buildroot Config.in replacement with custom packages added
BUILDROOT_CONFIG_IN=$HERE/os/configs/buildroot.Config.in
BUILDROOT_FS_CONFIG_IN=$HERE/os/configs/fs.Config.in
BUILDROOT_EXT3_CONFIG_DIR=$HERE/os/configs/ext3

BUILDROOT_CONFIG=$HERE/os/configs/buildroot.config
BUSYBOX_CONFIG=$HERE/os/configs/busybox.config
KERNEL_CONFIG=$HERE/os/configs/linux-deadline.config
UCLIBC_CONFIG=$HERE/os/configs/uClibc.config

# Where do we keep custom buildroot packages?
BUILDROOT_CUSTOM_PACKAGE_DIR=${HERE}/buildroot-packages

# Which custom packages do we want to install?
BUILDROOT_CUSTOM_PACKAGES="libcg libdeadline schedtool-dl libselinux policycoreutils libsemanage ustr checkpolicy refpolicy sepolgen"

# Directories we need know about.  There shouldn't be any need to
# change these.
KERNEL_DIR=$BUILD/linux-deadline
LIBDEADLINE_DIR=$BUILD/libdl
SCHEDTOOL_DIR=$BUILD/schedtool-dl
BUILDROOT_DIR=$BUILD/buildroot-${BUILDROOT_RELEASE}

function command_exists {
    which $1 >/dev/null 2>&1
}

function install_distro_packages {
    echo "[dist] Installing distribution packages"

    if command_exists apt-get
    then
        sudo apt-get install -y swig libpcre3-dev bc libaio-dev gawk
    elif command_exists yum
    then
        sudo yum install -y swig pcre-devel bc libaio-devel
    else
        echo "Unknown platform; yum and apt-get not found"
	exit 1
    fi
}

function update-repo {
    local url=$1
    local branch=$2
    local dir=$3

    if [ -d ${dir} ]
    then
        echo "[git] Updating repository ${dir}"
        pushd ${dir}
        git pull
        popd
    else
        echo "[git] Cloning repository ${url}"
        git clone ${url} -b ${branch} ${dir}
    fi
}

function update-repos {
    update-repo ${KERNEL_REPO} ${KERNEL_BRANCH} ${KERNEL_DIR}
    update-repo ${LIBDEADLINE_REPO} ${LIBDEADLINE_BRANCH} ${LIBDEADLINE_DIR}
    update-repo ${SCHEDTOOL_REPO} ${SCHEDTOOL_BRANCH} ${SCHEDTOOL_DIR}
}

function update-submodules {
    git submodule init
    git submodule update
}

function unpack-buildroot {
    if [ ! -d ${BUILDROOT_DIR} ]
    then
        echo "[buildroot] fetching and unpacking ${BUILDROOT_PKG}"
        pushd $BUILD
        wget -c ${BUILDROOT_URL}
        tar -xf ${BUILDROOT_PKG}
        popd
    else
        echo "[buildroot] NOTICE: skipping buildroot unpack since ${BUILDROOT_DIR} already exists"
    fi
}

function unpack-refpolicy {
    if [ ! -d ${REFPOLICY_DIR} ]
    then
        echo "[refpolicy] fetching and unpacking ${REFPOLICY_PKG}"
        pushd $BUILD
        wget -c ${REFPOLICY_URL}
        tar -xf ${REFPOLICY_PKG}
        popd
    else
        echo "[refpolicy] NOTICE: skipping refpolicy unpack since ${REFPOLICY_DIR} already exists"
    fi
}

function install-configs {
    local buildroot_dest=${BUILDROOT_DIR}/.config
    local kernel_dest=${KERNEL_DIR}/.config

    echo "[configs] Installing buildroot config at $buildroot_dest"
    cp ${BUILDROOT_CONFIG} ${buildroot_dest}

    if [ ! -f $kernel_dest ]
    then
        echo "[configs] Installing linux-deadline kernel config at $kernel_dest"
        cp ${KERNEL_CONFIG} ${kernel_dest}
    else
        echo "[configs] NOTICE: skipping installation of kernel config since ${kernel_dest} already exists"
    fi

    # Always install the busybox config.in file.
    # local busybox_config_in=${BUILDROOT_DIR}/package/busybox/Config.in
    # cp -f ${HERE}/configs/busybox.Config.in ${busybox_config_in}

    # Always install the busybox build script.
    local busybox_mk_in=${BUILDROOT_DIR}/package/busybox/busybox.mk
    cp -f ${HERE}/os/configs/busybox.mk ${busybox_mk_in}

    # Install changes to device mounting process
    local inittab_in=${BUILDROOT_DIR}/overlay/etc/inittab
    mkdir -p `dirname ${inittab_in}`
    cp -f ${HERE}/os/configs/inittab ${inittab_in}

    install-busybox-config
    install-uclibc-config
}

function install-busybox-config {
    local full_path=${BUILDROOT_DIR}/package/busybox/busybox.config
    echo "[busybox] Installing busybox configuration at $full_path"
    cp -f ${BUSYBOX_CONFIG} $full_path
}

function install-uclibc-config {
    # Note that this is the configured source path of the default
    # uClibc config as per the buildroot config; if you change the
    # buildroot config to use a different default uClibc config, this
    # will have no effect.
    local full_path=${BUILDROOT_DIR}/package/uclibc/uClibc-0.9.33.config
    echo "[uClibc] Installing uClibc configuration at $full_path"
    cp -f ${UCLIBC_CONFIG} $full_path
}

function install-custom-packages {
    # Always install the Config.in file (it should be harmless to do
    # this).  The Config.in file needs to be consistent with the
    # custom packages being installed, or else their Config.in files
    # won't be imported and buildroot won't know about them.
    cp -f ${BUILDROOT_CONFIG_IN} ${BUILDROOT_DIR}/package/Config.in

    # Install symlinks for custom buildroot packages into the package/
    # directory.
    for package in $BUILDROOT_CUSTOM_PACKAGES
    do
        local link_dest=$BUILDROOT_DIR/package/$package

        if [ ! -L $BUILDROOT_DIR/package/$package ]
        then
            echo "[buildroot] Creating symlink for custom package '$package' at $link_dest"
            cd $BUILDROOT_DIR/package && \
              ln -s ../../../buildroot-packages/$package && \
              cd -
        else
            echo "[buildroot] NOTICE: skipping symlink for package '$package', already exists at $link_dest"
        fi

        if ! grep "package/$package/Config.in" ${BUILDROOT_CONFIG_IN} >/dev/null
        then
            echo "[buildroot] WARNING: package '$package' is not mentioned in ${BUILDROOT_CONFIG_IN}"
        fi
    done

    cp -f ${HERE}/os/configs/libcap-ng.mk ${BUILDROOT_DIR}/package/libcap-ng/
    cp -f ${HERE}/os/configs/libcgroup.mk ${BUILDROOT_DIR}/package/libcgroup/
}

mkdir -p $BUILD

(install_distro_packages || echo "Could not install distribution packages without sudo privileges")
update-repos
update-submodules
unpack-buildroot
unpack-refpolicy
install-configs
install-custom-packages
