all: fasta.ghc-2.hs
	ghc --make -O2 -XBangPatterns -threaded -rtsopts -XOverloadedStrings fasta.ghc-2.hs -o fasta.ghc-2.ghc_run

clean:
	rm -f *hi *~ *.ghc_run *.o
