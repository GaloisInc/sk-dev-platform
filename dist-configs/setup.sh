#!/usr/bin/env bash

# This script sets up the environment necessary to build software from
# the Separation Kernel Platform distribution.  You should source this
# script rather than running it directly so that it mutates your
# current shell environment.

HERE=$(pwd)

# This is to make the toolchain and tool programs accessible.
PATH=$HERE/toolchain/bin:$HERE/tools:$PATH

# This is to make the toolchain binaries link correctly.
export LD_LIBRARY_PATH=$HERE/toolchain/lib

export BASE_INITRAMFS_SOURCE=$HERE/images
export BASE_INITRAMFS_BASENAME=base-initramfs
export CHECKSYMS=$HERE/tools/checksyms/checksyms.py
export CHECKSYMS_LIBC_WHITELIST=$HERE/tools/checksyms/libc_whitelist
export CHECKSYMS_LIBC=$HERE/toolchain/x86_64-unknown-linux-uclibc/sysroot/lib/libc.so.0
export TOOLCHAIN_PATH=$HERE/toolchain/bin/

