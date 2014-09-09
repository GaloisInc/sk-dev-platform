
#ifndef __LIBSK_NETWORK_H__
#define __LIBSK_NETWORK_H__

#include <sys/socket.h>
#include <netdb.h>
#include <netinet/ip.h>
#include <sys/types.h>
#include "channels.h"

// Constants.
// For the payload size, see the UDP message length information at
// http://en.wikipedia.org/wiki/User_Datagram_Protocol#Packet_structure
#define PAYLOAD_SIZE                 65507
#define NETWORK_MESSAGE_SIZE         sizeof(struct netmsg)

// Network device server types.
#define NETWORK_DEVICE_SEND          1
#define NETWORK_DEVICE_RECV          2

/*
 * A network message.  Depending on context, this is either a message
 * being received or sent.
 */
struct netmsg {
    in_addr_t address;
    unsigned short port;

    unsigned char payload[PAYLOAD_SIZE];
    size_t payload_size;
};

struct netdev {
    read_channel_p chan_recv;
    write_channel_p chan_send;
};

typedef struct netdev* netdev_p;
typedef struct netmsg* netmsg_p;

#endif
