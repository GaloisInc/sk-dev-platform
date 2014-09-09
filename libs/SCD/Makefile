##############################################################################
# SCD Makefile
# (c) 2007 Galois, Inc.
##############################################################################

CABAL = runhaskell Setup.hs
# BNFC isn't actually used
# BNFC  = bnfc
REFPOL = ../Reference-Policy

ifdef PROFILING
  CONFIG_OPTIONS=--enable-library-profiling --disable-library-vanilla
  TEST_BUILD_ARGS=-prof -auto-all -osuf p_o
  TEST_RTS_ARGS=+RTS $(PROFILING) -i5 -RTS
endif

.PHONY: default
default:
	$(MAKE) build

.PHONY: clean
clean::
	$(CABAL) clean

.PHONY: configure
configure:
	$(CABAL) configure --user --happy-options=--strict $(CONFIG_OPTIONS)
	rm -f src/SCD/SELinux/Test/data/policy.conf
	cd src/SCD/SELinux/Test/data && ln -s ../../../../../$(REFPOL)/refpolicy/policy.conf

.PHONY: build
build: configure
	$(CABAL) build

.PHONY: install
install: build
	$(CABAL) install

.PHONY: test
test: install
	rm -f dist/All All.tix lobster.tix
	ghc -package scd $(TEST_BUILD_ARGS) -Wall --make src/SCD/Test/All.hs -o dist/All
	time dist/All +RTS -K20M -RTS $(TEST_RTS_ARGS) $(TEST_ARGS)

callgraph.dot: install
	scd-m4-callgraph -dot $(REFPOL)/refpolicy/policy/modules > callgraph.dot || (rm -f callgraph.dot; false)

callgraph.ncol: install
	scd-m4-callgraph -ncol $(REFPOL)/refpolicy/policy/modules > callgraph.ncol || (rm -f callgraph.ncol; false)

callgraph.svg: callgraph.dot
	time neato -Gmodel=subset -v -Tsvg callgraph.dot -o callgraph.svg


.PHONY: doc
doc:
	$(CABAL) haddock

.PHONY: all
all:
#	$(MAKE) clean
	$(MAKE) coverage
#	$(MAKE) doc

# The coverage target will exercise the configure, build, install and
# test targets.

# TODO: Find a way to separate the hpc/no-hpc builds.  Currently, we
# need to "make clean" before switching between them.

# XML parameter overview
dump-parameters: install
	scd-xml --per-parameter $$(find $(REFPOL)/refpolicy/policy/modules -name \*.if)

dump-all-parameters: install
	scd-xml --all-parameters $$(find $(REFPOL)/refpolicy/policy/modules -name \*.if)

.PHONY: coverage
coverage:
	$(MAKE) test TEST_BUILD_ARGS=-fhpc CONFIG_OPTIONS=--ghc-options=-fhpc
# 	hpc report All.tix
# 	hpc markup --destdir=.hpc All.tix
	hpc report lobster.tix
	hpc markup --destdir=.hpc lobster.tix

clean::
	rm -rf .hpc All.tix lobster.tix src/SCD/Test/All.{hi,o,p_o} dist dist-hpc callgraph.dot callgraph.svg
