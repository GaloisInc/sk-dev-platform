# Installation directories.
PREFIX ?= $(DESTDIR)/usr
BINDIR ?= $(PREFIX)/bin

CFLAGS ?= -Wall -W -Wundef -Wshadow -Wmissing-noreturn -Wmissing-format-attribute
override CFLAGS += -I../include
LDLIBS += -L../src -lsepol 

TARGETS=$(patsubst %.c,%,$(wildcard *.c))

all: $(TARGETS)

install: all
	-mkdir -p $(BINDIR)
	install -m 755 $(TARGETS) $(BINDIR)

clean:
	-rm -f $(TARGETS) *.o 

indent:
	../../scripts/Lindent $(wildcard *.[ch])

relabel:

