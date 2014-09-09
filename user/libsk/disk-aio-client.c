
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <inttypes.h>
#include <assert.h>
#include <sys/stat.h>

#include "disk-aio.h"
#include "channels.h"

#define DEBUG         0
#define dprintf       if (DEBUG) printf

static struct disk_client_slot *get_slot(struct disk_client *c, int slot) {
    assert(slot >= 0 && slot < MAX_CLIENT_REQUESTS);
    return &(c->requests[slot]);
}

static int next_free_slot(struct disk_client *c) {
    int slot = -1;

    for (slot = 0; slot < MAX_CLIENT_REQUESTS; slot++) {
	if (c->request_slots[slot] == 0)
	    break;
    }

    return slot;
}

static int alloc_slot(struct disk_client *c) {
    int slot = next_free_slot(c);

    if (slot >= 0) {
	memset(&(c->requests[slot]), 0, sizeof(struct disk_client_slot));
	c->request_slots[slot] = 1;
    }

    dprintf("Allocated slot %d\n", slot);

    return slot;
}

static void free_slot(struct disk_client *c, int slot) {
    assert(c->request_slots[slot] == 1);
    c->request_slots[slot] = 0;
    dprintf("Freed slot %d\n", slot);
}

int disk_client_init(write_channel_p req_chan, read_channel_p resp_chan,
		     struct disk_client *client)
{
    if (resp_chan->ch.message_size != sizeof(struct disk_msg)) {
        dprintf("Specified response channel's message size does not match disk_msg size %ld\n",
		DISK_MESSAGE_SIZE);
        return 1;
    }

    if (req_chan->ch.message_size != sizeof(struct disk_msg)) {
        dprintf("Specified request channel's message size does not match disk_msg size %ld\n",
		DISK_MESSAGE_SIZE);
        return 1;
    }

    memset(client, 0, sizeof(struct disk_client));
    client->req_chan = req_chan;
    client->resp_chan = resp_chan;

    return 0;
}

int disk_client_read(struct disk_client *c, size_t num_sectors,
		     unsigned long start_sector,
		     read_callback_t cb, error_callback_t err)
{
    int slot_num, status;
    struct disk_client_slot *slot;

    if (channel_can_send(c->req_chan) && next_free_slot(c) >= 0) {
	slot_num = alloc_slot(c);
	slot = get_slot(c, slot_num);

	slot->cb.read = cb;
	slot->err = err;
	slot->msg.type = DISK_READ;
	slot->msg.start_sector = start_sector;
	slot->msg.num_sectors = num_sectors;
	slot->msg.handle = slot_num;

	status =channel_send(c->req_chan, &(slot->msg));
	if (status == CHANNEL_OK) {
	    return DISK_OK;
	} else {
	    printf("Error in channel send (read): %d\n", status);
	    return DISK_ERROR;
	}
    }

    return DISK_BUSY;
}

int disk_client_write(struct disk_client *c, size_t num_sectors,
		      unsigned long start_sector, unsigned char *buf,
		      write_callback_t cb, error_callback_t err)
{
    int slot_num, status;
    struct disk_client_slot *slot;

    if (channel_can_send(c->req_chan) && next_free_slot(c) >= 0) {
	slot_num = alloc_slot(c);
	slot = get_slot(c, slot_num);

	slot->cb.write = cb;
	slot->err = err;
	slot->msg.type = DISK_WRITE;
	slot->msg.start_sector = start_sector;
	slot->msg.num_sectors = num_sectors;
	slot->msg.handle = slot_num;

	memcpy(slot->msg.payload, buf, num_sectors * SECTOR_SIZE);

	status = channel_send(c->req_chan, &(slot->msg));
	if (status == CHANNEL_OK) {
	    return DISK_OK;
	} else {
	    printf("Error in channel send (write): %d\n", status);
	    return DISK_ERROR;
	}
    }

    return DISK_BUSY;
}

int disk_client_get_size(struct disk_client *c, size_callback_t cb, error_callback_t err)
{
    int slot_num, status;
    struct disk_client_slot *slot;

    if (channel_can_send(c->req_chan) && next_free_slot(c) >= 0) {
	slot_num = alloc_slot(c);
	slot = get_slot(c, slot_num);

	slot->cb.size = cb;
	slot->err = err;
	slot->msg.type = DISK_SIZE;
	slot->msg.handle = slot_num;

	status = channel_send(c->req_chan, &(slot->msg));
	if (status == CHANNEL_OK) {
	    return DISK_OK;
	} else {
	    printf("Error in channel send (size): %d\n", status);
	    return DISK_ERROR;
	}
    }

    return DISK_BUSY;
}

int disk_client_poll(struct disk_client *c)
{
    struct disk_msg msg;
    struct disk_client_slot *slot;
    int slot_num;

    while (channel_can_recv(c->resp_chan)) {
	channel_recv(c->resp_chan, &msg);

	slot_num = msg.handle;
	slot = get_slot(c, slot_num);

	if (msg.status != 0) {
	    slot->err(&msg);
	} else {
	    if (msg.type == DISK_SIZE) {
		slot->cb.size(msg.device_size);
	    } else if (msg.type == DISK_READ) {
		slot->cb.read(msg.num_sectors, msg.start_sector, msg.payload);	    } else {
		slot->cb.write(msg.num_sectors, msg.start_sector);
	    }
	}

	free_slot(c, slot_num);
    }

    return 0;
}
