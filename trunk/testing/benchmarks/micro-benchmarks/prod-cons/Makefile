all: mlton ghc host par

mlton: prod-cons-sml-seq.mlb
	mlton prod-cons-sml-seq.mlb

host: prod-cons-sml-host.sml
	../../../../build/bin/mlton prod-cons-sml-host.sml

par: prod-cons-sml-par.sml
	../../../../build/bin/mlton prod-cons-sml-par.sml

ghc: prod-cons-ghc.hs
	ghc --make -O2 -threaded prod-cons-ghc.hs -o prod-cons-ghc

clean:
	rm -f prod-cons-sml-seq prod-cons-sml-host prod-cons-ghc prod-cons-sml-par *.o *.hi *~
