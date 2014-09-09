
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

#include "net2_driver.h"
#include "channels.h"
#include "network.h"
#include "network_server_lwip.h"

void usage(const char *progname) {
    printf("Usage: %s <interface IP> <gateway> <netmask> <dest IP> <dest port>\n",
	   progname);
}

int cell_main(int argc, char **argv) {
    unsigned short port;

    if (argc != 6) {
	usage(argv[0]);
	return 1;
    }

    if (sscanf(argv[5], "%hd", &port) != 1) {
	usage(argv[0]);
	return 1;
    }

    printf("Sending to port: %hd\n", port);

    return run_netdev_lwip(argv[1], argv[2], argv[3],
    			   0, NULL,
			   argv[4], port, from_pep);
}
