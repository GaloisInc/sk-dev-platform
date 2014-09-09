
#ifndef __LIBSK_NETWORK_SERVER_UNIX_H__
#define __LIBSK_NETWORK_SERVER_UNIX_H__

#include <sys/socket.h>
#include <netdb.h>
#include <netinet/ip.h>
#include <sys/types.h>
#include "channels.h"

#include "network.h"

/* Server API */
int run_netdev_unix(const char *dest_hostname, const char *dest_port,
		    read_channel_p send_src,
		    const char *listen_hostname, const char *listen_port,
		    write_channel_p recv_dest);

#endif
