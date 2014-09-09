
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "disk1_driver.h"
#include "channels.h"
#include "disk-aio.h"

int cell_main(int argc, char **argv) {
    return run_disk_async(to_disk1, from_disk1, "disk1.dat");
}
