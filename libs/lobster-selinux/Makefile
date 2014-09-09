##############################################################################
# Lobster SELinux back-end Makefile
# (c) 2009 Galois, Inc.
##############################################################################

CABAL = runhaskell Setup.hs

.PHONY: default
default:
	$(MAKE) build

.PHONY: clean
clean:
	$(CABAL) clean
	rm -f test/examples/*.te test/examples/*.fc

.PHONY: configure
configure:
	$(CABAL) configure --user

.PHONY: build
build: configure
	$(CABAL) build

.PHONY: install
install: build
	$(CABAL) install

.PHONY: test
test: build
	dist/build/lobster-selinux-test/lobster-selinux-test

.PHONY: doc
doc:
	$(CABAL) haddock
