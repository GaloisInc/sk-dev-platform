
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#include "disk.h"
#include "channels.h"

#ifdef DEBUG
#define dprintf printf
#else
#define dprintf if (0) printf
#endif

struct disk {
    read_channel_p chan_in;
    write_channel_p chan_out;
    int fd;
    off_t device_size;
    const char *disk_file_path;
};

static int disk_set_size(struct disk *dsk) {
    struct stat st;

    if (fstat(dsk->fd, &st) == -1) {
        dprintf("Failed to get disk size: %s\n", strerror(errno));
        return errno;
    }

    dsk->device_size = st.st_size;
    dprintf("Got disk size = %ld bytes\n", st.st_size);
    return 0;
}

static int handle_size_message(struct disk *dsk, struct disk_message * msg) {
    msg->body.device_size = dsk->device_size;
    return 0;
}

static int handle_read(struct disk *dsk, struct disk_io_request * req) {
    dprintf("Reading %ld bytes at position %ld\n",
            req->num_sectors * SECTOR_SIZE, req->start_sector * SECTOR_SIZE);
    lseek(dsk->fd, req->start_sector * SECTOR_SIZE, SEEK_SET);
    return read(dsk->fd, req->payload, req->num_sectors * SECTOR_SIZE);
}

static int handle_write(struct disk *dsk, struct disk_io_request * req) {
    dprintf("Writing %ld bytes at position %ld\n",
            req->num_sectors * SECTOR_SIZE, req->start_sector * SECTOR_SIZE);
    lseek(dsk->fd, req->start_sector * SECTOR_SIZE, SEEK_SET);
    return write(dsk->fd, req->payload, req->num_sectors * SECTOR_SIZE);
}

static int handle_io_message(struct disk *dsk, struct disk_message * msg) {
    struct disk_io_request *req;

    req = &(msg->body.io_request);

    if (req->num_sectors > MAX_SECTORS_PER_REQUEST) {
        dprintf("Got invalid IO request for %ld sectors\n", req->num_sectors);
        return 1;
    }

    if ((req->start_sector + req->num_sectors) > (dsk->device_size / SECTOR_SIZE)) {
        dprintf("IO request for invalid sector(s): num_sectors=%ld, start_sector=%ld, total device size is %ld\n",
                req->num_sectors, req->start_sector, dsk->device_size);
        return 1;
    }

    dprintf("IO request: type %d, start_sector %ld, num_sectors %ld\n",
            req->type, req->start_sector, req->num_sectors);

    if (req->type == DISK_IO_REQUEST_READ) {
        return handle_read(dsk, req);
    } else if (req->type == DISK_IO_REQUEST_WRITE) {
        return handle_write(dsk, req);
    } else {
        dprintf("Got invalid IO request type: %d\n", req->type);
        return 1;
    }
}

static int handle_message(struct disk *dsk, struct disk_message * msg) {
    if (msg->type == DISK_MSG_GET_SIZE) {
        return handle_size_message(dsk, msg);
    } else if (msg->type == DISK_MSG_IO_REQUEST) {
        return handle_io_message(dsk, msg);
    } else {
        dprintf("Got invalid disk message type: %d\n", msg->type);
        return 1;
    }
}

/*
 * Runs a disk service on the specified disk file, using chan_in to
 * receive disk service commands and chan_out to send responses to
 * those commands.
 *
 * This service will always respond to messages in the order in which
 * they are received.  The channels provided must use a message size
 * of EXACTLY sizeof(struct disk_message).  chan_in MUST be a recv
 * channel and chan_out MUST be a send channel.  The underlying disk
 * file MUST be readable and writable and MUST already exist.
 *
 * Returns 0 on success or 1 on any kind of failure.
 */
int run_disk(read_channel_p chan_in, write_channel_p chan_out, const char *disk_file_path) {
    struct disk dsk;
    int status;
    struct disk_message msg;

    if (chan_in->ch.message_size != sizeof(struct disk_message)) {
        dprintf("Specified input channel's message size does not match disk_message size (%ld bytes)\n",
		sizeof(struct disk_message));
        return 1;
    }

    if (chan_out->ch.message_size != sizeof(struct disk_message)) {
        dprintf("Specified output channel's message size does not match disk_message size (%ld bytes)\n",
		sizeof(struct disk_message));
        return 1;
    }

    memset(&dsk, 0, sizeof(dsk));
    dsk.chan_in = chan_in;
    dsk.chan_out = chan_out;
    dsk.disk_file_path = disk_file_path;

    dsk.fd = open(disk_file_path, O_RDWR);
    if (dsk.fd == -1) {
        dprintf("Could not open disk file: %s\n", strerror(errno));
        return 1;
    }

    if (disk_set_size(&dsk))
        return 1;

    while (1) {
        status = channel_recv(chan_in, &msg);
        if (status != 0) {
            dprintf("Error receiving message on input channel: %d\n", status);
            return 1;
        }

        msg.status = handle_message(&dsk, &msg);

        status = channel_send(chan_out, &msg);
        if (status != 0) {
            dprintf("Error sending message on output channel: %d\n", status);
            return 1;
        }
    }

    return 0;
}
