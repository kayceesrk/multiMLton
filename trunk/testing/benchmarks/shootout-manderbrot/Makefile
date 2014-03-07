all: mlton multiMLton ghc java c ghc-cml

mlton: mandelbrot-sml-seq.sml
	mlton mandelbrot-sml-seq.sml

multiMLton: mandelbrot-sml-par.sml
	../../../build/bin/mlton mandelbrot-sml-par.sml

ghc: mandelbrot.ghc-2.hs
	ghc --make -O2 -threaded -XBangPatterns -optc-mfpmath=sse -optc-msse2 -rtsopts  mandelbrot.ghc-2.hs -o mandelbrot.ghc-2.ghc_run

ghc-cml: mandelbrot-ghc-cml.hs
	ghc --make -O2 -threaded -XBangPatterns -optc-mfpmath=sse -optc-msse2 -rtsopts $<


java:
	javac mandelbrot.java

c:
	gcc -pipe -Wall -O3 -fomit-frame-pointer -march=native -std=c99 -mfpmath=sse -msse2 -fopenmp mandelbrot.gcc-3.c -o mandelbrot.gcc-3.gcc_run

clean:
	rm -f mandelbrot-sml-seq mandelbrot-sml-par mandelbrot.ghc-2.ghc_run *.o *~ *.class *.hi *.gcc_run *-ghc-cml
