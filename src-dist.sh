#!/usr/bin/env bash

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

git clone $REPO_URL $DIST_DIR

pushd $DIST_DIR
bash setup.sh
git submodule init
git submodule update

rm $DIST_DIR/os/build/*.gz
rm $DIST_DIR/os/build/*.bz2
rm $DIST_DIR/src-dist.sh
rm $DIST_DIR/docs/RELEASES
rm $DIST_DIR/os/README
rm -f $DIST_DIR/os/build/buildroot-2014.08/dl.tar
rm -rf $DIST_DIR/libs
rm -rf $DIST_DIR/.git
rm $DIST_DIR/.gitmodules
find $DIST_DIR -name .gitignore | xargs rm

rm -rf $DIST_DIR/os/build/linux-deadline/.git

pushd $DIST_DIR/..
tar -czv $DIST_NAME > $HERE/$DIST_PKG
popd
popd
rm -rf $DIST_DIR
