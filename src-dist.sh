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

mv $DIST_DIR/dist-configs/grub.conf.sample $DIST_DIR/
mv $DIST_DIR/docs/programming_guide.txt $DIST_DIR/
mv $DIST_DIR/docs/README.src.dist $DIST_DIR/README
mv $DIST_DIR/LICENSE $DIST_DIR/

rm $DIST_DIR/os/build/*.gz
rm $DIST_DIR/os/build/*.bz2
rm $DIST_DIR/{src-dist.sh,hudson-build.sh,upload.sh}
rm $DIST_DIR/docs/RELEASES
rm $DIST_DIR/os/README
rm -f $DIST_DIR/os/build/buildroot-2014.08/dl.tar
rm -rf $DIST_DIR/dist-configs
rm -rf $DIST_DIR/docs
rm -rf $DIST_DIR/libs
rm -rf $DIST_DIR/.git
rm $DIST_DIR/.gitmodules
find $DIST_DIR -name .gitignore | xargs rm

cd $DIST_DIR/os/build/libdl/ && git remote rm origin
cd $DIST_DIR/os/build/schedtool-dl/ && git remote rm origin
cd $DIST_DIR/os/build/linux-deadline/ && git remote rm origin

rm -rf $DIST_DIR/os/build/linux-deadline/.git

pushd $DIST_DIR/..
tar -czv $DIST_NAME > $HERE/$DIST_PKG
popd
popd
rm -rf $DIST_DIR
