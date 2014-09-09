#!/bin/bash

echo "Patching up the filesystem!"

for x in bin sbin usr/bin usr/sbin; do
  CC=output/host/usr/bin/x86_64-unknown-linux-uclibc-gcc runghc ../../utils/BusyboxWrapper.hs output/target/$x
done

for x in var/log var/run etc/resolv.conf ; do
  if [ -L output/target/$x ] ; then
    rm    output/target/$x ;
    mkdir output/target/$x ;
  fi
done
