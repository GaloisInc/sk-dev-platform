
#ifndef __LIBSK_NETWORK_C__
#define __LIBSK_NETWORK_C__

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <errno.h>
#include "channels.h"
#include "network.h"

#define DEBUG          1
#define dprintf        if (DEBUG) printf

/* Client API */

int netdev_can_recv(netdev_p nd) {
    return (nd != NULL &&
	    nd->chan_recv != NULL &&
	    channel_can_recv(nd->chan_recv));
}

int netdev_can_send(netdev_p nd) {
    return (nd != NULL &&
	    nd->chan_send != NULL &&
	    channel_can_send(nd->chan_send));
}

netdev_p netdev_setup(read_channel_p recv_chan, write_channel_p send_chan) {
    netdev_p nd = NULL;

    nd = malloc(sizeof(struct netdev));
    if (nd == NULL) {
	dprintf("netdev: could not allocate memory\n");
	return NULL;
    }

    if (recv_chan == NULL && send_chan == NULL) {
	free(nd);
	dprintf("netdev: both recv and send channel were null\n");
	return NULL;
    }

    if (recv_chan != NULL) {
	if (recv_chan->ch.message_size != NETWORK_MESSAGE_SIZE) {
	    free(nd);
	    dprintf("netdev: recv channel had the wrong message size (expected %ld bytes)\n",
		    NETWORK_MESSAGE_SIZE);
	    return NULL;
	}
    }

    if (send_chan != NULL) {
	if (send_chan->ch.message_size != NETWORK_MESSAGE_SIZE) {
	    free(nd);
	    dprintf("netdev: send channel had the wrong message size (expected %ld bytes)\n",
		    NETWORK_MESSAGE_SIZE);
	    return NULL;
	}
    }

    nd->chan_recv = recv_chan;
    nd->chan_send = send_chan;

    return nd;
}

int netdev_try_recv(netdev_p nd, netmsg_p msg) {
    return channel_try_recv(nd->chan_recv, msg);
}

int netdev_try_send(netdev_p nd, netmsg_p msg) {
    return channel_try_send(nd->chan_send, msg);
}

int netdev_recv(netdev_p nd, netmsg_p msg) {
    return channel_recv(nd->chan_recv, msg);
}

int netdev_send(netdev_p nd, netmsg_p msg) {
    return channel_send(nd->chan_send, msg);
}

#endif
