all: mlton multiMLton ghc

mlton: threadring-sml-seq.mlb
	mlton threadring-sml-seq.mlb

host: threadring-sml-host.sml
	../../../build/bin/mlton threadring-sml-host.sml

par: threadring-sml-par.sml
	../../../build/bin/mlton threadring-sml-par.sml

ghc: threadring-ghc.hs
	ghc --make -O2 -threaded -XBangPatterns -rtsopts threadring-ghc.hs

java:
	javac threadring.java

c:
	gcc -pipe -Wall -O3 -fomit-frame-pointer -march=native -std=c99 -mfpmath=sse -msse2 -fopenmp threadring.gcc-3.c -o threadring.gcc-3.gcc_run

clean:
	rm -f threadring-sml-seq threadring-sml-par threadring-sml-host threadring-ghc *.o *~ *.class *.hi *.gcc_run
