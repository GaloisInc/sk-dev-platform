clean:
	rm -f signatures *.out

signatures:
	grep "readFile ::" Lobster.hs >signatures
	grep "writeFile ::" Lobster.hs >>signatures
	grep "putStrLn ::" Lobster.hs >>signatures
	grep "openFile ::" Lobster.hs >>signatures
	grep "systeme ::" Lobster.hs >>signatures
	grep "external ::" Lobster.hs >>signatures
	grep "type LobsterAST" Lobster.hs >>signatures
	grep "data PortName" Lobster.hs >>signatures
	grep "data ConnSpec" Lobster.hs >>signatures

%: %.hs
	ghc --make $<
