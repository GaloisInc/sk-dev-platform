
#ifndef __LIBSK_DISK_H__
#define __LIBSK_DISK_H__

#include <sys/types.h>
#include "channels.h"

// Constants.
#define SECTOR_SIZE                  512
#define MAX_SECTORS_PER_REQUEST      8
#define DISK_MESSAGE_SIZE            sizeof(struct disk_message)

// Message types.
#define DISK_MSG_IO_REQUEST          0
#define DISK_MSG_GET_SIZE            1

// IO request types.
#define DISK_IO_REQUEST_READ         0
#define DISK_IO_REQUEST_WRITE        1

/*
 * A request for disk I/O.
 *
 * If 'type' is DISK_REQUEST_READ, the 'num_sectors' sectors starting
 * at sector 'start_sector' will be read into the 'payload' buffer.
 *
 * If 'type' is DISK_REQUEST_WRITE, the payload buffer's contents (up
 * to num_sectors * SECTOR_SIZE) will be written to the underlying
 * disk at sector 'start_sector'.
 */
struct disk_io_request {
    // The type of the request: either DISK_REQUEST_READ or
    // DISK_REQUEST_WRITE.
    unsigned char type;

    // The number of sectors to be read or written.
    size_t num_sectors;

    // The offset at which to read or write.
    unsigned long start_sector;

    // The buffer containing the sectors to be written, or the
    // destination buffer for sectors to be read.
    unsigned char payload[SECTOR_SIZE * MAX_SECTORS_PER_REQUEST];
};

struct disk_message {
    // The message type.  Either DISK_MSG_IO_REQUEST or
    // DISK_MSG_GET_SIZE.
    unsigned char type;

    union {
        struct disk_io_request io_request;
        unsigned long device_size;
    } body;

    // The result of the operation; 0 for success or non-zero for
    // failure.
    unsigned char status;
};

/*
 * Run the disk driver process; accept requests from the input channel
 * 'chan_in' and send responses on the output channel 'chan_out'.  Use
 * the file 'disk_file_path' as the underlying storage.
 */
int run_disk(read_channel_p chan_in, write_channel_p chan_out, const char *disk_file_path);

#endif
