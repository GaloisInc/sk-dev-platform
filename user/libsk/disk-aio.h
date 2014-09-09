
#ifndef __LIBSK_DISK_AIO_H__
#define __LIBSK_DISK_AIO_H__

/*
 * Asynchronous disk I/O library
 *
 * Usage:
 *
 * Both client and server are responsible for setting up channel_p
 * pointers to channels with message sizes set to DISK_MESSAGE_SIZE
 * and directionality set according to the requirements of the
 * client/server setup functions (see below).
 *
 * The server cell runs run_disk_async() to service I/O requests for
 * sectors within a specified disk file.
 *
 * The client creates a disk_client structure, initializes it with
 * disk_client_init(), submits read, write, and size requests with the
 * provided functions, calls disk_client_poll() periodically to
 * process server responses, and provides callback functions to handle
 * responses.
 *
 * When the client issues a request for a read, only the sector
 * parameters are given.  When the response arrives and the handling
 * callback is invoked, it is passed a pointer to the read dat which
 * _must_ be copied to its final destination by the user-provided
 * callback.
 *
 * When the client issues a request for a write, both the sector
 * parameters and write buffer are given.  When the response arrives
 * and the handling callback is invoked, it is only passed the sector
 * parameters.
 *
 * The client and server have fixed capacity for pending requests and
 * responses.  If the client attempts to send a request when the disk
 * client or server's capacity has been exhausted, the relevant
 * function will return DISK_BUSY instead of DISK_OK.  The client is
 * responsible for trying again until DISK_OK is returned; it may be
 * necessary to purge pending responses with disk_client_poll().
 */

#include <sys/types.h>
#include <inttypes.h>
#include "channels.h"

// Constants.
#define SECTOR_SIZE                  512
#define MAX_SECTORS_PER_REQUEST      8
#define DISK_MESSAGE_SIZE            sizeof(struct disk_msg)
#define MAX_CLIENT_REQUESTS          128

// Message types.
#define DISK_READ                    0
#define DISK_WRITE                   1
#define DISK_SIZE                    2

// Status values for disk operations.
#define DISK_OK                      0
#define DISK_BUSY                    1
#define DISK_ERROR                   2

/*
 * An asynchronous disk I/O message used for both requests and
 * responses.
 */
struct disk_msg {
    // Request: the message type.  DISK_MSG_READ, DISK_MSG_WRITE, or
    // DISK_MSG_GET_SIZE.
    unsigned char type;

    // Request: the number of sectors to be read or written.
    size_t num_sectors;

    // Request: the offset at which to read or write.
    unsigned long start_sector;

    // Request/response: the buffer containing the sectors to be
    // written (request), or the destination buffer for sectors to be
    // read (response).
    unsigned char payload[SECTOR_SIZE * MAX_SECTORS_PER_REQUEST];

    // Response: the size of the device in a device size response.
    unsigned long device_size;

    // Response: the status of the operation.  Zero indicates success;
    // non-zero indicates failure.  Specific non-zero values are
    // undefined.
    unsigned int status;

    // Request/response: an opaque client-provided handle for
    // identifying the response/request parity.  This field is handled
    // internally by the client-side implementation and the server
    // side never touches it.
    unsigned int handle;
};

/*
 * The type of read response callback functions.
 */
typedef void (*read_callback_t)(size_t num_sectors,
				unsigned long start_sector,
				unsigned char *buf);

/*
 * The type of write response callback functions.
 */
typedef void (*write_callback_t)(size_t num_sectors,
				 unsigned long start_sector);

/*
 * The type of disk size response callback functions.
 */
typedef void (*size_callback_t)(unsigned long device_size);

/*
 * The type of disk error callback functions.  The server response
 * message will be passed as the first argument.
 */
typedef void (*error_callback_t)(struct disk_msg *msg);

/*
 * Internal structure for representing client request states.
 */
struct disk_client_slot {
    union {
	read_callback_t read;
	write_callback_t write;
	size_callback_t size;
    } cb;

    error_callback_t err;

    struct disk_msg msg;
};

/*
 * Disk client structure.  Set this up with disk_client_init() and
 * pass it to all disk_client_* functions.
 */
struct disk_client {
    write_channel_p req_chan;
    read_channel_p resp_chan;
    struct disk_client_slot requests[MAX_CLIENT_REQUESTS];
    uint8_t request_slots[MAX_CLIENT_REQUESTS];
};

/* Client API */

/*
 * Initialize a client structure from request and response channels.
 * The request and response channels MUST have the correct types
 * (CHANNEL_TYPE_SEND for the request channel, CHANNEL_TYPE_RECV for
 * the response channel) and MUST have the correct message sizes
 * (DISK_MESSAGE_SIZE).  Returns 0 on success, or 1 on failure to meet
 * the above conditions.
e */
int disk_client_init(write_channel_p req_chan, read_channel_p resp_chan,
		     struct disk_client *client);

/*
 * Submit a request to read num_sectors sectors from the disk,
 * starting at start_sector, with the provided callbacks to be invoked
 * on success or failure.  Returns DISK_OK on success, DISK_ERROR on
 * error, or DISK_BUSY if the request queue is full.
 */
int disk_client_read(struct disk_client *client, size_t num_sectors,
		     unsigned long start_sector,
		     read_callback_t cb, error_callback_t err);

/*
 * Submit a request to write num_sectors to the disk, starting at
 * start_sector, using data from buf, with the provided callbacks to
 * be invoked on success or failure.  Returns DISK_OK on success,
 * DISK_ERROR on error, or DISK_BUSY if the request queue is full.
 */
int disk_client_write(struct disk_client *client, size_t num_sectors,
		      unsigned long start_sector, unsigned char *buf,
		      write_callback_t cb, error_callback_t err);

/*
 * Submit a request to get the size of the disk, in sectors, with the
 * provided callbacks to * be invoked on success or failure.  Returns
 * DISK_OK on success, DISK_ERROR on error, or DISK_BUSY if the
 * request queue is full.
 */
int disk_client_get_size(struct disk_client *client, size_callback_t cb,
			 error_callback_t err);

/*
 * Poll the disk client for pending server responses and invoke their
 * callbacks to process the results.  Always succeeds (returns 0).
 */
int disk_client_poll(struct disk_client *client);

/* Server API */

/*
 * Run a disk server using the specified channels and the specified
 * underlying filesystem path to a file.  The request and response
 * channels MUST have the correct types (CHANNEL_TYPE_SEND for the
 * response channel, CHANNEL_TYPE_RECV for the request channel) and
 * MUST have the correct message sizes (DISK_MESSAGE_SIZE).  Returns 0
 * on success, or 1 on failure to meet the above conditions.
 */
int run_disk_async(read_channel_p request_channel, write_channel_p response_channel,
		   const char *disk_file_path);

#endif
