CABAL = runhaskell Setup.hs

.PHONY: default
default:
	$(MAKE) build

.PHONY: clean
clean::
	$(CABAL) clean

.PHONY: configure
configure:
	$(CABAL) configure --user

.PHONY: build
build: configure
	$(CABAL) build

.PHONY: install
install: build
	$(CABAL) install
