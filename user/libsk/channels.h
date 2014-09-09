
#ifndef __LIBSK_CHANNELS_H__
#define __LIBSK_CHANNELS_H__

#include <sys/types.h>
#include "mem.h"

// The amount of memory required for the specified message size / slot
// count.
#define REQUIRED_SIZE(msg_size, num_slots) \
    (msg_size * num_slots)

/*
 * Channel types.
 */
#define CHANNEL_TYPE_SEND                 1
#define CHANNEL_TYPE_RECV                 2

/*
 * Channel operation status codes.
 */
#define CHANNEL_OK                        0
#define CHANNEL_INVALID                   -1
#define CHANNEL_EMPTY                     -2
#define CHANNEL_FULL                      -3
#define CHANNEL_INVALID_SLOT              -4
#define CHANNEL_INVALID_TYPE              -5
#define CHANNEL_INVALID_STATUS            -6
#define CHANNEL_NO_MATCHING_SLOT          -7

/*
 * Slot status values.
 */
#define SLOT_STATUS_FREE                  0
#define SLOT_STATUS_USED                  1

/*
 * Overwrite constants passed to write_channel.
 */
#define CHAN_NO_OVERWRITES                0
#define CHAN_ALLOW_OVERWRITES             1

struct channel_base {
    struct mem slots;
    struct mem reader_metadata;
    struct mem writer_metadata;

    size_t message_size;
    size_t num_slots;
};

struct read_channel {
    struct channel_base ch;
    int *last_read;
    int *num_read;
    int *last_written;
    int *num_written;
};

struct write_channel {
    struct channel_base ch;
    int *last_read;
    int *last_written;
    int *num_written;
    int *num_read;
    int allow_overwrites;
};

typedef struct read_channel* read_channel_p;
typedef struct write_channel* write_channel_p;

write_channel_p write_channel(struct mem messages, struct mem reader_meta,
			      struct mem writer_meta, size_t msg_size,
			      size_t num_slots, int allow_overwrites);

read_channel_p read_channel(struct mem messages, struct mem reader_meta,
			    struct mem writer_meta, size_t msg_size,
			    size_t num_slots);

// Send a message.
int channel_send(write_channel_p chan, void *buf);

// Receive a message.
int channel_recv(read_channel_p chan, void *buf);

// Return 1 if the channel has room to send.
int channel_can_send(write_channel_p chan);

// Return 1 if the channel has messages ready to be received.
int channel_can_recv(read_channel_p chan);

int channel_try_recv(read_channel_p chan, void *buf);
int channel_try_send(write_channel_p chan, void *buf);

#endif
