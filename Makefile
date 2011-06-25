GHC = ghc
GHC_FLAGS = -fforce-recomp -itest -isrc
INCLUDE = --include=Text.CSS.CSSParser

tests:
	$(GHC) $(GHC_FLAGS) -fhpc --make -main-is TestSuite test/TestSuite.hs
	rm -f TestSuite.tix
	./test/TestSuite

hpc-text:
	hpc report TestSuite $(INCLUDE)

hpc-markup:
	hpc markup TestSuite $(INCLUDE)
	open hpc_index.html

cov: tests hpc-markup

docs:
	cabal configure
	cabal haddock
	open dist/doc/html/css-parser/index.html

clean:
	rm -f example
	rm -f test/TestSuite
	rm -f `find . -type f -name *.hi`
	rm -f `find . -type f -name *.o`
	rm -f *.html
	rm -f TestSuite.tix
	rm -rf .hpc
	cabal clean
