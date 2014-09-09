
#ifndef __LIBSK_NETWORK_SERVER_LWIP_H__
#define __LIBSK_NETWORK_SERVER_LWIP_H__

#include "channels.h"
#include "network.h"

int run_netdev_lwip(const char *if_ip, const char *if_gw, const char *if_netmask,
		    const unsigned short listen_port, write_channel_p recv_dest,
		    const char *dest_ip, const unsigned short dest_port,
		    read_channel_p send_src);

#endif
