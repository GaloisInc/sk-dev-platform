#
# Makefile for building the checkpolicy program
#
PREFIX ?= $(DESTDIR)/usr
BINDIR ?= $(PREFIX)/bin
MANDIR ?= $(PREFIX)/share/man
# LIBDIR ?= $(PREFIX)/lib
LIBDIR ?= ../libsepol-2.0.41/src/
INCLUDEDIR ?= $(PREFIX)/include
TARGETS = checkpolicy checkmodule

YACC = bison -y

CFLAGS ?= -g -Wall -Wshadow -O2 -pipe -fno-strict-aliasing

override CFLAGS += -I. -I${INCLUDEDIR} -I../libsepol-2.0.41/include

CHECKOBJS = y.tab.o lex.yy.o queue.o module_compiler.o parse_util.o \
	    policy_define.o
CHECKPOLOBJS = $(CHECKOBJS) checkpolicy.o
CHECKMODOBJS = $(CHECKOBJS) checkmodule.o

# On Fedora 16, if configuration fails because $(LD) cannot find -lfl (below),
# then try:
#
# 1. yum install compat-flex
# 2. cd /usr/lib[64]
# 3. sudo ln -s flex-2.5.4a/libfl.a ./libfl.a
#
# This is likely sub-optimal, but works. -- AAM
LDLIBS=$(LIBDIR)/libsepol.a -lfl

GENERATED=lex.yy.c y.tab.c y.tab.h

all:  $(TARGETS)
#	$(MAKE) -C test

checkpolicy: $(CHECKPOLOBJS)

checkmodule: $(CHECKMODOBJS)

%.o: %.c 
	$(CC) $(CFLAGS) -o $@ -c $<

y.tab.o: y.tab.c
	$(CC) $(filter-out -Werror, $(CFLAGS)) -o $@ -c $<

lex.yy.o: lex.yy.c
	$(CC) $(filter-out -Werror, $(CFLAGS)) -o $@ -c $<

y.tab.c: policy_parse.y
	$(YACC) -d policy_parse.y

lex.yy.c: policy_scan.l y.tab.c
	$(LEX) policy_scan.l

install: all
	-mkdir -p $(BINDIR)
	-mkdir -p $(MANDIR)/man8
	install -m 755 $(TARGETS) $(BINDIR)	
	install -m 644 checkpolicy.8 $(MANDIR)/man8
	install -m 644 checkmodule.8 $(MANDIR)/man8

relabel: install
	/sbin/restorecon $(BINDIR)/checkpolicy
	/sbin/restorecon $(BINDIR)/checkmodule

clean:
	-rm -f $(TARGETS) $(CHECKPOLOBJS) $(CHECKMODOBJS) y.tab.c y.tab.h lex.yy.c
	$(MAKE) -C test clean

indent:
	../scripts/Lindent $(filter-out $(GENERATED),$(wildcard *.[ch]))
