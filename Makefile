
all: problems

problems: problems.hs Dice.hs Infer.hs Tally.hs Gamble.hs
	ghc -XPackageImports --make problems.hs

clean: 
	rm -f *.hi *.o problems 

