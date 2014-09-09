
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "cell2_driver.h"
#include "channels.h"
#include "disk.h"

int cell_main(int argc, char **argv) {
    struct disk_message *msg;
    int status;
    unsigned long disk_size;

    msg = malloc(DISK_MESSAGE_SIZE);

    // Query the disk to get its size so we know how to identify
    // invalid writes.
    msg->type = DISK_MSG_GET_SIZE;
    if ((status = channel_send(to_disk2, msg))) {
        printf("[cell2] Could not send disk message: %d\n", status);
        return 1;
    } else
        printf("[cell2] sent size request\n");

    if ((status = channel_recv(from_disk2, msg))) {
        printf("[cell2] Could not get disk reply: %d\n", status);
        return 1;
    }

    if (msg->type != DISK_MSG_GET_SIZE) {
        printf("[cell2] Something bad happened; got non-size reply (type %d)\n",
               msg->type);
        return 1;
    }

    disk_size = msg->body.device_size;

    // Loop forever, receiving disk read messages from the neighboring
    // cell, and converting them to writes and sending them to this
    // cell's disk.
    while (1) {
        if ((status = channel_recv(middle, msg))) {
            printf("[cell2] Could not recv data from cell 1: %d\n", status);
            return 1;
        }

        msg->body.io_request.type = DISK_IO_REQUEST_WRITE;

        if (msg->body.io_request.start_sector > (disk_size / SECTOR_SIZE)) {
            printf("[cell2] Ignoring write for sector %ld for disk size %ld\n",
                   msg->body.io_request.start_sector, disk_size);
            continue;
        }

        if ((status = channel_send(to_disk2, msg))) {
            printf("[cell2] Could not send disk write for sector %ld: %d\n",
                   msg->body.io_request.start_sector, status);
            return 1;
        }

        if ((status = channel_recv(from_disk2, msg))) {
            printf("[cell2] Could not receive disk read result: %d\n", status);
            return 1;
        }
    }

    return 0;
}
