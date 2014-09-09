
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "cell1_driver.h"
#include "channels.h"
#include "disk-aio.h"

struct disk_client client;
unsigned long g_device_size = 0;

void finished_read(size_t num_sectors, unsigned long start_sector,
		   unsigned char *buf) {
    struct disk_msg msg;

    printf("Got read response; sending to cell2\n");

    msg.type = DISK_WRITE;
    msg.start_sector = start_sector;
    msg.num_sectors = num_sectors;

    memcpy(msg.payload, buf, num_sectors * SECTOR_SIZE);

    while (channel_send(forward, &msg) == CHANNEL_FULL) {}
}

void got_size(unsigned long device_size) {
    printf("In size callback; size is %ld\n", device_size);
    g_device_size = device_size;
}

void got_error(struct disk_msg *msg) {
    printf("In error callback.\n");
    printf("Message type %d\n", msg->type);
    printf("Message status %d\n", msg->status);
}

int cell_main(int argc, char **argv) {
    unsigned long sector;

    // Initialize disk client
    if (disk_client_init(to_disk1, from_disk1, &client) != 0) {
	printf("Error initializing disk client\n");
	return 1;
    }

    // Submit request for size
    if (disk_client_get_size(&client, got_size, got_error)) {
	printf("Error requesting size\n");
	return 1;
    }

    // Wait for size response
    while (g_device_size == 0) {
	disk_client_poll(&client);
    }

    sector = 0;

    // Loop forever, reading sectors from disk one at a time and
    // sending them to the neighboring cell.  When we reach the end of
    // the disk, start over.
    while (1) {
	while (disk_client_read(&client, 1, sector, finished_read, got_error) == DISK_BUSY) {}
        sector = (sector + 1) % (g_device_size / SECTOR_SIZE);
	disk_client_poll(&client);
    }

    return 0;
}
