
#ifndef __LIBSK_NETWORK_UNIX_C__
#define __LIBSK_NETWORK_UNIX_C__

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

/* Server API */

int run_netdev_unix(const char *dest_hostname, const char *dest_port,
		    read_channel_p send_src,
		    const char *listen_hostname, const char *listen_port,
		    write_channel_p recv_dest) {

    struct netmsg msg;
    int server_sock = 0, client_sock = 0, flags = 0;
    struct addrinfo *dest_info, *listen_info;
    struct addrinfo hints;
    struct sockaddr fromaddr;
    socklen_t fromaddrlen;

    if (recv_dest == NULL && send_src == NULL) {
	printf("[netdev] Error: one or both of recv/send channels must be provided\n");
	return 1;
    }

    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_DGRAM;
    hints.ai_protocol = IPPROTO_UDP;
    hints.ai_canonname = NULL;
    hints.ai_addr = NULL;
    hints.ai_next = NULL;

    // Set up the client socket, if appropriate.
    if (send_src != NULL) {
	if (getaddrinfo(dest_hostname, dest_port, &hints, &dest_info) != 0) {
	    printf("Error: %s (%d)\n", strerror(errno), errno);
	    return 1;
	}

	client_sock = socket(AF_INET, SOCK_DGRAM|SOCK_NONBLOCK, IPPROTO_UDP);
	if (client_sock == -1) {
	    printf("[netdev] Error in client socket creation: %s (%d)\n",
		   strerror(errno), errno);
	    return errno;
	}

	if (connect(client_sock, dest_info->ai_addr, dest_info->ai_addrlen) == -1) {
	    printf("[netdev] Error in client connect: %s (%d)\n",
		   strerror(errno), errno);
	    return errno;
	}

	freeaddrinfo(dest_info);
    }

    if (recv_dest != NULL) {
	if (getaddrinfo(listen_hostname, listen_port, &hints, &listen_info) != 0) {
	    printf("Error: %s (%d)\n", strerror(errno), errno);
	    return 1;
	}

	server_sock = socket(AF_INET, SOCK_DGRAM|SOCK_NONBLOCK, IPPROTO_UDP);
	if (server_sock == -1) {
	    printf("[netdev] Error in server socket creation: %s (%d)\n",
		   strerror(errno), errno);
	    return errno;
	}

	if (bind(server_sock, listen_info->ai_addr, listen_info->ai_addrlen) == -1) {
	    printf("[netdev] Error in server bind: %s (%d)\n", strerror(errno),
		   errno);
	    return errno;
	}

	freeaddrinfo(listen_info);
    }

    flags = MSG_DONTWAIT;

    while (1) {
	// Check to see if we can receive a message.  Only check for
	// one, though, and then check for sends, and repeat.  This
	// gives equal time to each and doesn't spend too much time
	// receiving and delaying sends.
	//
	// In effect we are dropping packets if we receive a network
	// message and the channel is full.
	if (recv_dest != NULL && channel_can_send(recv_dest)) {
	    msg.payload_size = recvfrom(server_sock, msg.payload, PAYLOAD_SIZE, flags,
					&fromaddr, &fromaddrlen);

	    if (msg.payload_size == -1) {
		if (errno != EWOULDBLOCK && errno != EAGAIN) {
		    printf("[netdev] Error in recv: %s (%d)\n", strerror(errno), errno);
		}
	    } else {
		dprintf("[netdev] got message, %ld bytes, sending to recv channel\n",
			msg.payload_size);

		msg.address = ((struct sockaddr_in *) &fromaddr)->sin_addr.s_addr;
		msg.port = ((struct sockaddr_in *) &fromaddr)->sin_port;

		channel_try_send(recv_dest, &msg);
	    }
	}

	if (send_src != NULL && channel_can_recv(send_src)) {
	    int ret;

	    ret = channel_try_recv(send_src, &msg);
	    if (ret != CHANNEL_OK) {
		if (ret != CHANNEL_EMPTY) {
		    dprintf("[netdev] error in channel_try_recv, got %d\n", ret);
		}
		continue;
	    }

	    dprintf("[netdev] sending without address (%ld bytes)\n", msg.payload_size);

	    ret = send(client_sock, &(msg.payload), msg.payload_size, flags);
	    if (ret == -1) {
		if (errno != EWOULDBLOCK && errno != EAGAIN) {
		    printf("[netdev] Error in send: %s (%d)\n", strerror(errno), errno);
		}
	    }
	}
    }

    return 0;
}

#endif
