
#ifndef __LIBSK_NETWORK_SERVER_UNIX_LWIP_C__
#define __LIBSK_NETWORK_SERVER_UNIX_LWIP_C__

#include <stdio.h>
#include <unistd.h>
#include <tapif.h>

#include <lwip/tcpip.h>
#include <lwip/netbuf.h>

#include "channels.h"
#include "network.h"
#include "network_server_lwip.h"

#define DEBUG 0
#define dprintf if (DEBUG) printf

struct conn_info {
    ip_addr_t dest_addr;
    unsigned short dest_port;
    ip_addr_t listen_addr;
    unsigned short listen_port;
};

struct conn_info info;

int parse_address(char* addr, struct addrinfo* info, int family) {
    struct addrinfo hints;
    struct addrinfo *result;
    struct addrinfo *r;
    int res;

    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = 0;
    hints.ai_protocol = 0;
    hints.ai_flags = AI_NUMERICSERV;
    res = getaddrinfo(addr, 0, &hints, &result);

    if (res != 0) {
	fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(res));
	return -1;
    }

    for (r = result; r != NULL; r = r->ai_next) {
	if (r->ai_family == family) {
	    *info = *r;
	    return 0;
	}
    }

    return -1;
}

int parse_ip4(char *ip_addr_str, struct ip_addr *dest)
{
    struct addrinfo addr;
    uint8_t *p;

    if (ip_addr_str == 0 || *ip_addr_str == 0)
      return -1;

    if (parse_address(ip_addr_str, &addr, AF_INET) != 0)
      return -1;

    p = (uint8_t*) &((struct sockaddr_in*)addr.ai_addr)->sin_addr;
    IP4_ADDR(dest, *p, *(p+1), *(p+2), *(p+3));

    return 0;
}

int parse_pair(struct tapif* tapif, char* key, char* value)
{
    if (key == 0 || *key == 0)
	return -1;

    if (!strcmp(key, "name")) {
	if (value == 0 || *value == 0)
	    return -1;
	tapif->name = value;
	return 0;
    } else if (!strcmp(key, "addr")) {
	return parse_ip4(value, &(tapif->ip_addr));
    } else if (!strcmp(key, "netmask")) {
	return parse_ip4(value, &(tapif->netmask));
    } else if (!strcmp(key, "gw")) {
	return parse_ip4(value, &(tapif->gw));
    } else {
	return -1;
    }
}

static void send_thread(void *arg)
{
    read_channel_p send_src = (read_channel_p) arg;
    struct netmsg msg;
    static struct netconn *conn;
    static struct netbuf *buf;
    int ret;

    dprintf("Sender thread starting up\n");

    conn = netconn_new(NETCONN_UDP);
    LWIP_ASSERT("con != NULL", conn != NULL);

    buf = netbuf_new();
    if (buf == NULL) {
	printf("ERROR: could not allocate netbuf in send thread\n");
	exit(1);
    }

    while (1) {
	if (channel_can_recv(send_src)) {
	    ret = channel_try_recv(send_src, &msg);
	    if (ret != CHANNEL_OK) {
		if (ret != CHANNEL_EMPTY) {
		    dprintf("[netdev] error in channel_try_recv, got %d\n", ret);
		}
		continue;
	    }

	    dprintf("[netdev] got packet (%ld bytes) from channel\n",
		    msg.payload_size);

	    netbuf_alloc(buf, msg.payload_size);

	    if ((ret = netbuf_take(buf, msg.payload, msg.payload_size))
		!= ERR_OK) {
		dprintf("[netdev] Error copying buffer of %ld bytes! (status %d)\n",
			msg.payload_size, ret);
		netbuf_free(buf);
		continue;
	    }

	    ret = netconn_sendto(conn, buf, &info.dest_addr, info.dest_port);
	    if (ret != ERR_OK) {
		printf("ERROR: could not send packet, got status %d\n", ret);
	    }

	    netbuf_free(buf);
	}
    }
}

static void recv_thread(void *arg)
{
    write_channel_p recv_dest = (write_channel_p) arg;
    struct netmsg msg;
    static struct netconn *conn;
    static struct netbuf *buf;
    int status;

    dprintf("Receiver thread starting up\n");

    conn = netconn_new(NETCONN_UDP);
    LWIP_ASSERT("con != NULL", conn != NULL);

    status = netconn_bind(conn, NULL, info.listen_port);
    if (status != ERR_OK) {
	printf("Error binding to listening port %d: %d\n", info.listen_port, status);
	exit(1);
    }

    dprintf("Receiver thread bound to port %d\n", info.listen_port);

    while (1) {
	if (channel_can_send(recv_dest)) {
	    dprintf("waiting on packet...\n");
	    status = netconn_recv(conn, &buf);
	    if (status == ERR_OK) {
		dprintf("[netdev] got message, %d bytes, sending to recv channel\n",
			buf->p->tot_len);

		status = netbuf_copy(buf, msg.payload, buf->p->tot_len);
		if (status != buf->p->tot_len) {
		    dprintf("[netdev] Error copying buffer payload! (got status %d, expected %d)\n",
			    status, buf->p->tot_len);
		    continue;
		}

		memcpy(&msg.address, &(buf->addr), sizeof(ip_addr_t));
		msg.port = buf->port;
		msg.payload_size = buf->p->tot_len;

		status = channel_try_send(recv_dest, &msg);
		if (status != CHANNEL_OK) {
		    dprintf("channel_try_send returned %d, expected %d\n",
			    status, CHANNEL_OK);
		} else {
		    dprintf("Sent message\n");
		}

		netbuf_delete(buf);
	    }
	} else {
	    dprintf("channel full\n");
	}
    }
}

int run_netdev_lwip(const char *if_ip, const char *if_gw, const char *if_netmask,
		    const unsigned short listen_port, write_channel_p recv_dest,
		    const char *dest_ip, const unsigned short dest_port,
		    read_channel_p send_src) {

    struct tapif tapif;
    struct netif netif;

    memset(&tapif, 0, sizeof(tapif));
    memset(&netif, 0, sizeof(netif));
    memset(&info, 0, sizeof(info));

    tcpip_init(NULL, NULL);

    if (recv_dest == NULL && send_src == NULL) {
	dprintf("Both recv_dest and send_src were null\n");
	return -1;
    }

    if (parse_pair(&tapif, "addr", (char*) if_ip) ||
	parse_pair(&tapif, "gw", (char*) if_gw) ||
	parse_pair(&tapif, "netmask", (char*) if_netmask)) {
	dprintf("One of the IP fields did not parse\n");
	return -1;
    }

    if (tapif.ip_addr.addr == IPADDR_ANY ||
	tapif.netmask.addr == IPADDR_ANY ||
	tapif.gw.addr == IPADDR_ANY) {
	dprintf("One of the IP fields was IPADDR_ANY\n");
        return -1;
    }

    dprintf("LWIP: ip %s, gw %s, netmask %s\n", if_ip, if_gw, if_netmask);

    if (recv_dest) {
	dprintf("Listening on: port %d\n", listen_port);

	info.listen_addr = tapif.ip_addr;
	info.listen_port = listen_port;

	dprintf("Spawning receiver thread\n");
	sys_thread_new("recv_thread", recv_thread, recv_dest,
		       DEFAULT_THREAD_STACKSIZE, DEFAULT_THREAD_PRIO);
    } else {
	dprintf("Not listening for incoming traffic\n");
    }

    if (send_src && dest_ip && dest_port != 0) {
	dprintf("Destination: %s:%d\n", dest_ip, dest_port);

	if (parse_ip4((char*) dest_ip, &(info.dest_addr)) != 0) {
	    printf("ERROR: destination IP '%s' invalid\n", dest_ip);
	    exit(1);
	}

	info.dest_port = dest_port;

	dprintf("Spawning sender thread\n");
	sys_thread_new("send_thread", send_thread, send_src,
		       DEFAULT_THREAD_STACKSIZE, DEFAULT_THREAD_PRIO);
    } else {
	dprintf("Not sending outgoing traffic\n");
    }

    netif_add(&netif,
	      &tapif.ip_addr,
	      &tapif.netmask,
	      &tapif.gw,
	      &tapif,
	      tapif_init,
	      tcpip_input);

    netif_set_default(&netif);
    netif_set_up(&netif);

    // Wait forever (practically).
    dprintf("Waiting while threads do work\n");
    pause();
    dprintf("Shutting down\n");

    return -1;
}

#endif
