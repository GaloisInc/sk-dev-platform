clean:
	rm -f *.fc *.te *.out a.in b.in *.o *.hi A B UseCase1 UseCase2

LOBSTER = ~/scd/SCD/dist/build/lobster/lobster

%.fc: %.lsr
	$(LOBSTER) -I Prelude.lsr $<
	mv module.fc $(*F).fc
	mv module.te $(*F).te

%: %.hs
	ghc --make $<

UseCase1.out: UseCase1.in UseCase1
	./UseCase1 UseCase1.in UseCase1.out

UseCase2.out: UseCase2.in A B UseCase2
	./UseCase2 UseCase2.in UseCase2.out
	@echo "=== Output file (UseCase2.out) follows ==="
	@cat UseCase2.out
	@echo "\n"
