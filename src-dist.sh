#!/usr/bin/env bash

# src-dist.sh
#
# This script creates a source-only distribution of the Separation Kernel
# development platform and accompanying source code repositories.  The output
# of this script is a compressed tarball,
#
#   separation-kernel-dev-platform-src-<VERSION>.tar.gz
#
# which includes all source code and configuration files pre-installed and
# ready to build.  Such distributions can be unpacked and built with the
# included "build.sh" script.

set -e

VERSION=$1

if [ -z "$VERSION" ]
then
    VERSION="dev"
fi

HERE=$(cd `dirname $0`; pwd)
DIST_NAME=separation-kernel-dev-platform-src-$VERSION
DIST_DIR=/tmp/$DIST_NAME
DIST_PKG=$DIST_NAME.tar.gz
REPO_URL=${REPO_URL:-https://github.com/GaloisInc/sk-dev-platform.git}

# Remove any preexisting distribution build directory
rm -rf $DIST_DIR

# Clone the development platform repository
git clone $REPO_URL $DIST_DIR

# Set it up by checking out additional repos and installing configuration files
# to prepare the repository for building
pushd $DIST_DIR
bash setup.sh

# Remove artifacts that we don't want or need to include in distributions,
# either to reduce the size or remove redundant information
rm $DIST_DIR/src-dist.sh
rm $DIST_DIR/os/build/*.gz
rm $DIST_DIR/os/build/*.bz2
rm $DIST_DIR/os/README
rm $DIST_DIR/.gitmodules
rm -rf $DIST_DIR/.git
rm -rf $DIST_DIR/os/build/linux-deadline/.git
find $DIST_DIR -name .gitignore | xargs rm

# Create the final archive
pushd $DIST_DIR/..
tar -czv $DIST_NAME > $HERE/$DIST_PKG
popd
popd
rm -rf $DIST_DIR
