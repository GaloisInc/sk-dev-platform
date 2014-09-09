
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "cell2_driver.h"
#include "channels.h"
#include "disk-aio.h"

unsigned long g_device_size = 0;
struct disk_client client;

void finished_write(size_t num_sectors, unsigned long start_sector) {
    printf("Got write response for start %ld, num %ld\n",
	   start_sector, num_sectors);
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
    struct disk_msg msg;
    int status;

    if (disk_client_init(to_disk2, from_disk2, &client) != 0) {
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

    // Loop forever, receiving disk read messages from the neighboring
    // cell, and converting them to writes and sending them to this
    // cell's disk.
    while (1) {
        if ((status = channel_recv(forward, &msg))) {
            printf("[cell2] Could not recv data from cell 1: %d\n", status);
            return 1;
        }

        if (msg.start_sector > (g_device_size / SECTOR_SIZE)) {
            printf("[cell2] Ignoring write for sector %ld for disk size %ld\n",
                   msg.start_sector, g_device_size);
            continue;
        }

	while (disk_client_write(&client, msg.num_sectors, msg.start_sector,
				 msg.payload, finished_write, got_error) == DISK_BUSY) {}

	disk_client_poll(&client);
    }

    return 0;
}
