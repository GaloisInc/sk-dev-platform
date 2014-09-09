
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

#include "net2_driver.h"
#include "channels.h"
#include "network.h"
#include "network_server_unix.h"

void usage(const char *progname) {
    printf("Usage: %s <dest IP> <dest port>\n", progname);
}

int cell_main(int argc, char **argv) {
    if (argc != 3) {
	usage(argv[0]);
	return 1;
    }

    return run_netdev_unix(argv[1], argv[2], from_pep, NULL, 0, NULL);
}
