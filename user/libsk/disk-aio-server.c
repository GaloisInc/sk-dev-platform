
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <assert.h>
#include <time.h>
#include <libaio.h>

#include "disk-aio.h"
#include "channels.h"

#define DEBUG           1
#define dprintf         if (DEBUG) printf

// Use the same maximum for events; we want to have one event per
// request (IOCB) in case they are all completed when we poll.
#define MAX_REQUESTS    128

struct disk_request_slot {
    struct iocb job;
    struct disk_msg msg;
};

struct disk {
    read_channel_p chan_in;
    write_channel_p chan_out;
    int fd;
    off_t device_size;
    const char *disk_file_path;
    io_context_t ctx;

    struct disk_request_slot requests[MAX_REQUESTS];
    unsigned char request_slots[MAX_REQUESTS];
    struct io_event events[MAX_REQUESTS];
};

static struct disk_request_slot *get_slot(struct disk *disk, int slot) {
    assert(slot >= 0 && slot < MAX_REQUESTS);
    return &(disk->requests[slot]);
}

static int next_free_slot(struct disk *disk) {
    int slot = -1;

    for (slot = 0; slot < MAX_REQUESTS; slot++) {
	if (disk->request_slots[slot] == 0)
	    break;
    }

    return slot;
}

static int alloc_slot(struct disk *disk) {
    int slot = next_free_slot(disk);

    if (slot >= 0) {
	memset(&(disk->requests[slot]), 0, sizeof(struct disk_request_slot));
	disk->request_slots[slot] = 1;
    }

    dprintf("Allocated slot %d\n", slot);

    return slot;
}

static void free_slot(struct disk *disk, int slot) {
    assert(disk->request_slots[slot] == 1);
    disk->request_slots[slot] = 0;
    dprintf("Freed slot %d\n", slot);
}

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

int run_disk_async(read_channel_p chan_in, write_channel_p chan_out, const char *disk_file_path) {
    struct disk dsk;
    int status, slot_num, i;
    struct timespec no_timeout;
    struct disk_request_slot *slot;
    struct io_event *e;

    if (chan_in->ch.message_size != sizeof(struct disk_msg)) {
        dprintf("Specified input channel's message size does not match disk_req size\n");
        return 1;
    }

    if (chan_out->ch.message_size != sizeof(struct disk_msg)) {
        dprintf("Specified output channel's message size does not match disk_req size\n");
        return 1;
    }

    dsk.chan_in = chan_in;
    dsk.chan_out = chan_out;
    dsk.disk_file_path = disk_file_path;

    dsk.fd = open(disk_file_path, O_RDWR);
    if (dsk.fd == -1) {
        dprintf("Could not open disk file: %s\n", strerror(errno));
        return 1;
    }

    if (disk_set_size(&dsk) != 0) {
	printf("Error: could not get disk size: %s (%d)\n", strerror(errno),
	       errno);
	return 1;
    }

    memset(&dsk.ctx, 0, sizeof(io_context_t));
    memset(dsk.requests, 0, MAX_REQUESTS * sizeof(struct disk_request_slot));
    memset(dsk.request_slots, 0, MAX_REQUESTS);
    memset(dsk.events, 0, MAX_REQUESTS * sizeof(struct io_event));

    // Be sure to zero the timeout structure: all zeros means
    // io_getevents should return instantly instead of blocking
    // forever.
    memset(&no_timeout, 0, sizeof(struct timespec));

    if ((status = io_queue_init(MAX_REQUESTS, &dsk.ctx)) != 0) {
	printf("Error in io_queue_init: %d\n", status);
	return status;
    }

    while (1) {
	if (next_free_slot(&dsk) >= 0 && channel_can_recv(chan_in)) {
	    slot_num = alloc_slot(&dsk);
	    slot = get_slot(&dsk, slot_num);

	    status = channel_try_recv(chan_in, &(slot->msg));
	    if (status == CHANNEL_OK) {
		dprintf("Received message into slot %d\n", slot_num);

		if (slot->msg.type == DISK_SIZE) {
		    // If a disk size request, get the device size and
		    // send a response (or wait until we can).  We
		    // don't send these responses asynchronously
		    // because we don't have a way to delay them.
		    slot->msg.device_size = dsk.device_size;

		    while (channel_try_send(dsk.chan_out, &(slot->msg)) == CHANNEL_FULL) {}

		    free_slot(&dsk, slot_num);
		} else if (slot->msg.type == DISK_READ || slot->msg.type == DISK_WRITE) {
		    if (slot->msg.type == DISK_READ) {

			io_prep_pread(&(slot->job), dsk.fd, slot->msg.payload,
				      slot->msg.num_sectors * SECTOR_SIZE,
				      slot->msg.start_sector * SECTOR_SIZE);

		    } else if (slot->msg.type == DISK_WRITE) {

			io_prep_pwrite(&(slot->job), dsk.fd, slot->msg.payload,
				       slot->msg.num_sectors * SECTOR_SIZE,
				       slot->msg.start_sector * SECTOR_SIZE);
		    }

		    slot->job.data = (void *)(intptr_t) slot_num;
		    struct iocb *ptr[1];
		    ptr[0] = &(slot->job);
		    status = io_submit(dsk.ctx, 1, ptr);
		    dprintf("Submitted iocb for slot %d, status %d\n", slot_num, status);
		} else {
		    dprintf("Warning: received disk I/O request with unsupported type: %d\n",
			    slot->msg.type);
		}
	    } else if (status != CHANNEL_EMPTY) {
		dprintf("Error receiving message on input channel: %d\n", status);
		return 1;
	    }
	}

	// Poll for events and send out responses.
	status = io_getevents(dsk.ctx, 0, MAX_REQUESTS, dsk.events, &no_timeout);

	if (status < 0) {
	    // Error.
	    dprintf("io_getevents: status %d\n", status);
	} else if (status > 0) {
	    for (i = 0; i < status; i++) {
		e = &(dsk.events[i]);
		slot_num = (int) (intptr_t) e->data;
		slot = get_slot(&dsk, slot_num);

		dprintf("Event: slot %d, status %ld, bytes %ld, type %d\n",
			slot_num, e->res2, e->res, slot->msg.type);

		// iocb is e->obj
		// slot_num is (int) e->data
		// e->res should be size of read/write in bytes
		// e->res2 should be zero on success, nonzero on failure

		slot->msg.status = e->res2;

		if (e->res != (slot->msg.num_sectors * SECTOR_SIZE)) {
		    printf("Error: event for %ld bytes didn't match request for %ld bytes\n",
			   e->res, slot->msg.num_sectors * SECTOR_SIZE);
		    slot->msg.status = -1;
		}

		while (channel_try_send(dsk.chan_out, &(slot->msg)) == CHANNEL_FULL) {}

		free_slot(&dsk, slot_num);
	    }
	}
    }

    return 0;
}
