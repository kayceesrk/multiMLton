all: mlton ghc host par

mlton: spawn-sml-seq.mlb
	mlton spawn-sml-seq.mlb

host: spawn-sml-host.sml
	../../../../build/bin/mlton spawn-sml-host.sml

par: spawn-sml-par.sml
	../../../../build/bin/mlton spawn-sml-par.sml

ghc: spawn-ghc.hs
	ghc --make -O2 -threaded spawn-ghc.hs -o spawn-ghc

clean:
	rm -f spawn-sml-seq spawn-sml-host spawn-ghc spawn-sml-par *.o *.hi *~
