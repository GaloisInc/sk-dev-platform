##############################################################################
# Lobster Makefile
# (c) 2009 Galois, Inc.
##############################################################################

BNFC = bnfc

# CABAL = runghc Setup.hs
# use a shared sandbox:
CABAL = cabal-dev --sandbox=../cabal-dev

BNFC_FILES = \
  src/Lobster/Abs.hs \
  src/Lobster/Doc.tex \
  src/Lobster/Doc.txt \
  src/Lobster/ErrM.hs \
  src/Lobster/Lex.x \
  src/Lobster/Par.y \
  src/Lobster/Print.hs \
  src/Lobster/Skel.hs \
  src/Lobster/Test.hs

.PHONY: default
default:
	$(MAKE) install # need to install for test suite to get built

.PHONY: clean
clean:
	rm -f $(BNFC_FILES)
	$(CABAL) clean
	rm -rf cabal-dev
#	(cd src/Lobster;make clean)
#	(cd src/Lobster/Test;make clean)
	rm -f test/examples/*.lobster test/examples/*.flatten

$(BNFC_FILES): src/Lobster/Lobster.cf
	rm -f $(BNFC_FILES)
	cd src/Lobster && $(BNFC) -d Lobster.cf
	mv src/Lobster/Lobster/*.* src/Lobster
	rmdir src/Lobster/Lobster

.PHONY: install
install: $(BNFC_FILES)
	$(CABAL) install

.PHONY: test
test: #src/Lobster/dist/build/lobster-parser-test/lobster-parser-test src/Lobster/Test/dist/build/lobster-test/lobster-test
	@for f in test/examples/example*.lsr ; do dist/build/lobster-parser-test/lobster-parser-test $$f > /dev/null && echo "Successfully parsed $$f" ; done
	dist/build/lobster-test/lobster-test

src/Lobster/dist/build/lobster-parser-test/lobster-parser-test :
	(cd src/Lobster;make)

src/Lobster/Test/dist/build/lobster-test/lobster-test :
	(cd src/Lobster/Test;make)

.PHONY: doc
doc:
	$(CABAL) haddock
