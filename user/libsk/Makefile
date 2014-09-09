
SONAME=sk
BASENAME=libsk
REALNAME=$(BASENAME).so.1.0

ARCH     =
AR       = $(TOOLCHAIN_PATH)$(ARCH)ar
AS       = $(TOOLCHAIN_PATH)$(ARCH)as
CC       = $(TOOLCHAIN_PATH)$(ARCH)gcc
CPP      = $(TOOLCHAIN_PATH)$(ARCH)cpp
CXX      = $(TOOLCHAIN_PATH)$(ARCH)g++
LD       = $(TOOLCHAIN_PATH)$(ARCH)ld
NM       = $(TOOLCHAIN_PATH)$(ARCH)nm
RANLIB   = $(TOOLCHAIN_PATH)$(ARCH)ranlib
OBJCOPY  = $(TOOLCHAIN_PATH)$(ARCH)objcopy
OBJDUMP  = $(TOOLCHAIN_PATH)$(ARCH)objdump
LDCONFIG = $(TOOLCHAIN_PATH)$(ARCH)ldconfig

LDFLAGS = -pthread
IP_VERSION = 4
CFLAGS=-Wall -fPIC -pthread -Wall -g -O2 \
  -Ilwip-tap -Ilwip-contrib/ports/unix/include \
  -Ilwip/src/include/ipv$(IP_VERSION) -Ilwip/src/include

OBJECTS=channels.o disk.o disk-aio-server.o disk-aio-client.o \
	network_client.o network_server_unix.o network_server_lwip.o

LWIP_SOURCES = \
  lwip/src/api/api_lib.c \
  lwip/src/api/api_msg.c \
  lwip/src/api/err.c \
  lwip/src/api/netbuf.c \
  lwip/src/api/netdb.c \
  lwip/src/api/netifapi.c \
  lwip/src/api/sockets.c \
  lwip/src/api/tcpip.c \
  lwip/src/core/def.c \
  lwip/src/core/dhcp.c \
  lwip/src/core/dns.c \
  lwip/src/core/init.c \
  lwip/src/core/mem.c \
  lwip/src/core/memp.c \
  lwip/src/core/netif.c \
  lwip/src/core/pbuf.c \
  lwip/src/core/raw.c \
  lwip/src/core/stats.c \
  lwip/src/core/sys.c \
  lwip/src/core/tcp.c \
  lwip/src/core/tcp_in.c \
  lwip/src/core/tcp_out.c \
  lwip/src/core/timers.c \
  lwip/src/core/udp.c \
  lwip/src/core/ipv4/autoip.c \
  lwip/src/core/ipv4/icmp.c \
  lwip/src/core/ipv4/igmp.c \
  lwip/src/core/ipv4/inet.c \
  lwip/src/core/ipv4/inet_chksum.c \
  lwip/src/core/ipv4/ip.c \
  lwip/src/core/ipv4/ip_addr.c \
  lwip/src/core/ipv4/ip_frag.c \
  lwip/src/core/snmp/asn1_dec.c \
  lwip/src/core/snmp/asn1_enc.c \
  lwip/src/core/snmp/mib2.c \
  lwip/src/core/snmp/mib_structs.c \
  lwip/src/core/snmp/msg_in.c \
  lwip/src/core/snmp/msg_out.c \
  lwip/src/netif/etharp.c \
  lwip-contrib/ports/unix/sys_arch.c \
  lwip-contrib/apps/chargen/chargen.c \
  lwip-contrib/apps/httpserver/httpserver-netconn.c \
  lwip-contrib/apps/tcpecho/tcpecho.c \
  lwip-contrib/apps/udpecho/udpecho.c \
  lwip-tap/tapif.c

LWIP_OBJECTS := $(foreach f,$(LWIP_SOURCES),$(f:.c=.o))
NODIR_LWIP_OBJECTS := $(foreach f,$(LWIP_SOURCES),$(notdir $(f:.c=.o)))

default: help

help:
	@echo "Targets:"
	@echo
	@echo "  host         Compile using the host toolchain"
	@echo "  cross 	      Compile using the buildroot cross compiler toolchain"
	@echo
	@echo "  clean        Clean up"

cross: TOOLCHAIN_PATH = ../../os/build/buildroot-2014.08/output/host/usr/bin/
cross: ARCH = x86_64-buildroot-linux-uclibc-
cross: build

host: TOOLCHAIN_PATH=
host: PATH := /sbin:${PATH}
host: build

build: shared static
	$(LDCONFIG) -n .

rebuild: clean
rebuild: default

%.o: %.c
	$(CC) $(CFLAGS) -c $<

shared: $(OBJECTS) $(LWIP_OBJECTS)
	$(CC) -shared -Wl,-soname,$(SONAME) -lpthread -laio -o $(REALNAME) $(OBJECTS) $(NODIR_LWIP_OBJECTS)

static: $(OBJECTS) $(LWIP_OBJECTS)
	$(AR) rcs $(BASENAME).a $(OBJECTS) $(NODIR_LWIP_OBJECTS)

clean:
	rm -f *.o *~ *.gch $(REALNAME) $(BASENAME).a sk $(NODIR_LWIP_OBJECTS)
