
all: dice

dice: dice.hs Infer.hs
	ghc -XPackageImports --make dice.hs

clean: dice
	rm -f *.hi *.o dice

