LSRFILES = $(wildcard *.lsr)
LOBSELFILES = $(addsuffix .lob.sel,$(LSRFILES))
CSSELFILES = $(addsuffix .cs.sel,$(LSRFILES))
CSM4FILES = $(addsuffix .cs.m4,$(LSRFILES))

.SECONDARY : $(CSM4FILES)

all : $(LOBSELFILES) $(CSSELFILES)

tests :
	../simplelobqc 100

# %.lsr.lob.sel : %.lsr
# 	../test/lobster.sh $< > $@

# %.lsr.cs.m4 : %.lsr ../sl2cs
# 	../test/sl2cs.sh $< > $@

# %.sel : %.m4
# 	m4 -I.. $< | grep -v \"^\$\" > $@

clean :
	rm -f *.m4 *.sel *.te *.fc *.out *.cso

vclean : clean
	rm -f *.lsr
