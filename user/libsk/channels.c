
#include <stdlib.h>
#include <sys/types.h>
#include <string.h>
#include <stdio.h>
#include "channels.h"
#include "mem.h"

#ifdef DEBUG
#define dprintf printf
#else
#define dprintf if (0) printf
#endif

write_channel_p write_channel(struct mem messages, struct mem reader_meta,
			      struct mem writer_meta, size_t msg_size,
			      size_t num_slots, int allow_overwrites) {

    write_channel_p chan;

    dprintf("Creating new write channel\n");

    if (num_slots <= 0) {
	dprintf("num_slots invalid\n");
        return NULL;
    }

    if (messages.size < REQUIRED_SIZE(msg_size, num_slots)) {
	dprintf("message buffer size (%z bytes) less than required size (%z bytes)\n",
		messages.size, REQUIRED_SIZE(msg_size, num_slots));
        return NULL;
    }

    // XXX check metadata sizes

    chan = malloc(sizeof(*chan));

    if (chan == NULL) {
	dprintf("Could not allocate memory\n");
        return NULL;
    }

    chan->ch.slots = messages;
    chan->ch.reader_metadata = reader_meta;
    chan->ch.writer_metadata = writer_meta;
    chan->ch.message_size = msg_size;
    chan->ch.num_slots = num_slots;

    chan->last_written = writer_meta.base;
    *chan->last_written = -1;
    chan->num_written = writer_meta.base + sizeof(*chan->last_written);
    *chan->num_written = 0;

    chan->last_read = reader_meta.base;
    chan->num_read = reader_meta.base + sizeof(*chan->last_read);

    dprintf("New write channel: base %p, size %z, msg size %z, slots %z.\n",
	    messages.base, messages.size, msg_size, num_slots);

    return chan;
}

read_channel_p read_channel(struct mem messages, struct mem reader_meta,
			    struct mem writer_meta, size_t msg_size,
			    size_t num_slots) {

    read_channel_p chan;

    dprintf("Creating new read channel\n");

    if (num_slots <= 0) {
	dprintf("num_slots invalid\n");
        return NULL;
    }

    if (messages.size < REQUIRED_SIZE(msg_size, num_slots)) {
	dprintf("message buffer size (%ld bytes) less than required size (%ld bytes)\n",
		messages.size, REQUIRED_SIZE(msg_size, num_slots));
        return NULL;
    }

    // XXX check metadata sizes

    chan = malloc(sizeof(*chan));

    if (chan == NULL) {
	dprintf("Could not allocate memory\n");
        return NULL;
    }

    chan->ch.slots = messages;
    chan->ch.reader_metadata = reader_meta;
    chan->ch.writer_metadata = writer_meta;
    chan->ch.message_size = msg_size;
    chan->ch.num_slots = num_slots;

    chan->last_read = reader_meta.base;
    *(chan->last_read) = -1;
    chan->num_read = reader_meta.base + sizeof(*chan->last_read);
    *chan->num_read = 0;

    chan->last_written = writer_meta.base;
    chan->num_written = writer_meta.base + sizeof(*chan->last_written);

    dprintf("New read channel: base %p, size %ld, msg size %ld, slots %ld.\n",
	    messages.base, messages.size, msg_size, num_slots);

    return chan;
}

/*
 * Get the pointer to the message address for the specified channel
 * and slot.
 */
static void * channel_slot_ptr(struct channel_base *chan, int slot) {
    if (chan == NULL)
        return NULL;

    if (slot < 0 || slot >= chan->num_slots)
        return NULL;

    return chan->slots.base + (chan->message_size * slot);
}

int write_chan_unread(write_channel_p chan) {
    return (*chan->num_written - *chan->num_read);
}

int read_chan_unread(read_channel_p chan) {
    return (*chan->num_written - *chan->num_read);
}

int channel_try_send(write_channel_p chan, void *buf) {
    int slot;

    if (chan == NULL)
        return CHANNEL_INVALID;

    if (!chan->allow_overwrites &&
	(write_chan_unread(chan) == chan->ch.num_slots)) {
	return CHANNEL_FULL;
    }

    slot = (*chan->last_written + 1) % chan->ch.num_slots;

    // Copy the message into the slot.
    memcpy(channel_slot_ptr(&(chan->ch), slot), buf, chan->ch.message_size);

    *chan->last_written = slot;
    (*chan->num_written)++;

    return CHANNEL_OK;
}

int channel_can_send(write_channel_p chan) {
    return (chan->allow_overwrites ||
	    (write_chan_unread(chan) < chan->ch.num_slots));
}

int channel_can_recv(read_channel_p chan) {
    return (read_chan_unread(chan) > 0);
}

int channel_try_recv(read_channel_p chan, void *buf) {
    int slot;

    if (chan == NULL)
        return CHANNEL_INVALID;

    if (read_chan_unread(chan) > chan->ch.num_slots) {
	// In this case, we've fallen behind by more than the buffer
	// size.  We "catch up" by resetting our num_read and
	// last_read counters so that the next read will be the most
	// recently written message, if any.
	(*chan->num_read) = (*chan->num_written);
	(*chan->last_read) = (*chan->last_written);
    }

    if (read_chan_unread(chan) == 0)
	return CHANNEL_EMPTY;

    slot = (*chan->last_read + 1) % chan->ch.num_slots;

    // Copy the message from the slot.
    memcpy(buf, channel_slot_ptr(&(chan->ch), slot), chan->ch.message_size);

    *chan->last_read = slot;
    (*chan->num_read)++;

    return CHANNEL_OK;
}

int channel_send(write_channel_p chan, void *buf) {
    int result;
    dprintf("channel_send: starting send on chan\n");
    while ((result = channel_try_send(chan, buf)) == CHANNEL_FULL) {}
    dprintf("channel_send: done, result %d\n", result);
    return result;
}

int channel_recv(read_channel_p chan, void *buf) {
    int result;
    dprintf("channel_recv: starting recv on chan\n");
    while ((result = channel_try_recv(chan, buf)) == CHANNEL_EMPTY) {}
    dprintf("channel_recv: done, result %d\n", result);
    return result;
}
