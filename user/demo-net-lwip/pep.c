
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "pep_driver.h"

#include "channels.h"
#include "network.h"
#include "network_client.h"

int safe(unsigned char *buf) {
    return (strncasecmp((char *) buf, "hello", 5) == 0);
}

int cell_main(int argc, char **argv) {
    netmsg_p msg;
    netdev_p nd;
    int status;

    msg = malloc(sizeof(struct netmsg));
    memset(msg, 0, sizeof(struct netmsg));
    nd = netdev_setup(to_pep, from_pep);

    if (nd == NULL) {
    	printf("Error setting up network client\n");
    	return 1;
    }

    while (1) {
        if ((status = netdev_recv(nd, msg))) {
	    printf("Error\n");
            return 1;
	}

	if (safe(msg->payload))
	    if ((status = netdev_send(nd, msg))) {
	    	printf("Error\n");
	    	return 1;
	    }
    }

    return 0;
}
