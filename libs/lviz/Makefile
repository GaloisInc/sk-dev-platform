CABAL = runhaskell Setup.hs
DATADIR = dist/build/share/lviz-0.1/

.PHONY: default
default:install

.PHONY: clean
clean::
	$(CABAL) clean

.PHONY: configure
configure:
	$(CABAL) configure --user

.PHONY: build
build: configure
	$(CABAL) build
	mkdir -p $(DATADIR)
	cp lviz.glade $(DATADIR)
	cp -ru help $(DATADIR)

.PHONY: install
install: build
	$(CABAL) install
