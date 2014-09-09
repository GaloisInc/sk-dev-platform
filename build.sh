#!/usr/bin/env bash

set -e

HERE=$(cd `dirname $0`; pwd)

# Build the sklite tool and run its test binary
cd $HERE/user/sklite
make clean
make

# Add sklite-* to the path
PATH=$HERE/user/sklite/.cabal-sandbox/bin:$PATH

# Bootstrap and build the 'buildroot' embedded linux distribution used
# to test our work
cd $HERE/os

make buildroot kernel

for d in $HERE/user/lib*
do
    make -C $d clean cross
done

# Build the demonstration programs
for demo in $HERE/user/demo*
do
    make -C $demo clean cross
done
