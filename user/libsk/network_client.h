
#ifndef __LIBSK_NETWORK_CLIENT_H__
#define __LIBSK_NETWORK_CLIENT_H__

#include "channels.h"
#include "network.h"

/* Client API */
int netdev_can_recv(netdev_p nd);
int netdev_can_send(netdev_p nd);
int netdev_recv(netdev_p nd, netmsg_p msg);
int netdev_send(netdev_p nd, netmsg_p msg);
int netdev_try_recv(netdev_p nd, netmsg_p msg);
int netdev_try_send(netdev_p nd, netmsg_p msg);
netdev_p netdev_setup(read_channel_p recv_chan, write_channel_p send_chan);

#endif
