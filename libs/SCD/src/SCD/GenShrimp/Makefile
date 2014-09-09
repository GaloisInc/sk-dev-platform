LANGS = SimpleLob CoreLob # CoreShrimp
MAINS = $(foreach lang, $(LANGS), Test$(lang)) sl2cs simplelobqc
.SECONDARY : $(foreach lang, $(LANGS), Lex$(lang).hs Par$(lang).hs)

all : doc main sl2cs

doc : $(foreach lang, $(LANGS), Doc$(lang).ps)
main : $(MAINS)

simplelobqc : SimpleLobQC.hs AbsSimpleLob.hs PrintSimpleLob.hs QC.hs
	ghc --make -o $@ $<

sl2cs : SL2CS.hs FromSimpleLob.hs FromCoreLob.hs LexSimpleLob.hs ParSimpleLob.hs AbsSimpleLob.hs ErrM.hs
	ghc --make -o $@ $<


Test% : Test%.hs Abs%.hs Lex%.x Par%.y Doc%.tex Skel%.hs Print%.hs Lex%.hs Par%.hs
	ghc --make $<

Abs%.hs Lex%.x Par%.y Doc%.tex Skel%.hs Print%.hs Test%.hs : %.cf # ErrM.hs
	bnfc $^

Lex%.hs : Lex%.x
	alex $<

Par%.hs : Par%.y
	happy $<

Doc%.dvi : Doc%.tex
	latex $^

Doc%.ps : Doc%.dvi
	dvips $^

clean :
	rm -f *.o *.hi $(foreach lang, $(LANGS), Abs$(lang).hs Lex$(lang).x Par$(lang).y Doc$(lang).tex Skel$(lang).hs Print$(lang).hs Test$(lang).hs Doc$(lang).ps Doc$(lang).aux Doc$(lang).log Doc$(lang).tex.bak  Lex$(lang).x.bak  Print$(lang).hs.bak  Test$(lang).hs.bak Par$(lang).y.bak  Skel$(lang).hs.bak Abs$(lang).hs.bak Lex$(lang).hs Par$(lang).hs) ErrM.hs ErrM.hs.bak $(MAINS)
