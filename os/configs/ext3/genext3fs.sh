#!/bin/sh

set -e
export LC_ALL=C

while getopts x:d:D:b:i:N:m:g:e:zfqUPhVv f
do
    case $f in
	d) TARGET_DIR=$OPTARG ;;
    esac
done

IMAGE="${!OPTIND}"

if [ -z "$TARGET_DIR" ]
then
    echo "No target directory specified!"
    exit 1
fi

INODES=$(find $TARGET_DIR | wc -l)
INODES=$(expr $INODES + 400)

# size ~= superblock, block+inode bitmaps, inodes (8 per block), blocks
# we scale inodes / blocks with 10% to compensate for bitmaps size + slack
BLOCKS=$(du -s -c -k $TARGET_DIR | grep total | sed -e "s/total//")
BLOCKS=$(expr 500 + \( $BLOCKS + $INODES / 8 \) \* 11 / 10)
BLOCKS=$(expr $BLOCKS \* 150 \/ 100)

# Create image file of appropriate size
dd if=/dev/zero of=$IMAGE bs=1024 count=$BLOCKS

# Make filesystem (ext3)
mke2fs -Fj $IMAGE

# Mount image
MOUNTPOINT=$(mktemp -d)
PIDFILE=$(mktemp)
guestmount -a $IMAGE --pid-file $PIDFILE --rw -m /dev/sda $MOUNTPOINT

pid=$(cat $PIDFILE)

# Copy contents of target directory into filesystem
cp -R $TARGET_DIR/* $MOUNTPOINT/
rm -R $MOUNTPOINT/dev/*

# Unmount image
fusermount -u $MOUNTPOINT
while kill -0 "$pid" 2>/dev/null
do
    sleep 1
done

rmdir $MOUNTPOINT
