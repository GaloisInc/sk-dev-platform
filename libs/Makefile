
.PHONY: lobster lobster-selinux lobster-xsm lobster-validate genLobster
.PHONY: scd shrimp lviz

all:
	cabal-dev install \
	  ./lobster \
  	  ./SCD \
	  ./lobster-selinux \
	  ./lobster-xsm \
	  ./lobster-validate \
	  ./shrimp \
	  ./genLobster

# ./lviz

clean :
	rm -rf cabal-dev
	(cd lobster && cabal clean)
	(cd SCD && cabal clean)
	(cd lobster-selinux && cabal clean)
	(cd lobster-xsm && cabal clean)
	(cd lobster-validate && cabal clean)
	(cd shrimp && cabal clean)
	(cd lviz && cabal clean)
	(cd genLobster && cabal clean)
