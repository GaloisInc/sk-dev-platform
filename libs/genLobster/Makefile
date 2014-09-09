CABAL = runhaskell Setup.hs

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

.PHONY: install
install: build
	$(CABAL) install

.PHONY: test
test: install test-refpolicy test-minipolicy

.PHONY: test-refpolicy
test-refpolicy:
	time genLobster --multiple dist/test/refpolicy ../../data/Reference-Policy/refpolicy
	for i in dist/test/refpolicy/*.lsr; do lobster-validate $$i || echo $$i failed; done

.PHONY: test-minipolicy
test-minipolicy:
	genLobster --multiple dist/test/MiniPolicy ../SCD/testsrc/SCD/M4/Test/MiniPolicy
	for i in dist/test/MiniPolicy/*.lsr; do lobster-validate $$i || echo $$i failed; done
