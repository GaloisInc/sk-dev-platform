
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "cell1_driver.h"
#include "channels.h"
#include "disk.h"

int cell_main(int argc, char **argv) {
    struct disk_message *msg;
    int status;
    unsigned long disk_size, sector;

    msg = malloc(DISK_MESSAGE_SIZE);

    // Query the disk to get its size so we know how to do our
    // reads.
    msg->type = DISK_MSG_GET_SIZE;
    if ((status = channel_send(to_disk1, msg))) {
        printf("[cell1] Could not send disk message: %d\n", status);
        return 1;
    } else
        printf("[cell1] sent size request\n");

    if ((status = channel_recv(from_disk1, msg))) {
        printf("[cell1] Could not get disk reply: %d\n", status);
        return 1;
    }

    if (msg->type != DISK_MSG_GET_SIZE) {
        printf("[cell1] Something bad happened; got non-size reply (type %d)\n",
               msg->type);
        return 1;
    }

    disk_size = msg->body.device_size;
    sector = 0;

    // Loop forever, reading sectors from disk one at a time and
    // sending them to the neighboring cell.  When we reach the end of
    // the disk, start over.
    while (1) {
        msg->type = DISK_MSG_IO_REQUEST;
        msg->body.io_request.type = DISK_IO_REQUEST_READ;
        msg->body.io_request.num_sectors = 1;
        msg->body.io_request.start_sector = sector;

        if ((status = channel_send(to_disk1, msg))) {
            printf("[cell1] Could not send disk read for sector %ld: %d\n", sector, status);
            return 1;
        }

        if ((status = channel_recv(from_disk1, msg))) {
            printf("[cell1] Could not receive disk read result: %d\n", status);
            return 1;
        }

        if ((status = channel_send(middle, msg))) {
            printf("[cell1] Could not send disk data to cell 2: %d\n", status);
            return 1;
        }

        sector = (sector + 1) % (disk_size / SECTOR_SIZE);
    }

    return 0;
}
