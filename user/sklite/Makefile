
TOP = $(shell pwd)
SANDBOX ?= $(TOP)/.cabal-sandbox
LIBS ?= $(TOP)/../../libs

RM = /bin/rm -rf
BUILD = cabal

.PHONY: sklite

all: sklite

sklite:
	cabal sandbox init
	$(BUILD) install

clean:
	$(RM) -rf $(SANDBOX)

