
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "disk2_driver.h"
#include "channels.h"
#include "disk.h"

int cell_main(int argc, char **argv) {
    return run_disk(to_disk2, from_disk2, "disk2.dat");
}
