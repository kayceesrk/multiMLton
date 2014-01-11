all: mlton multiMLton ghc java

mlton: mandelbrot-sml-seq.sml
	mlton mandelbrot-sml-seq.sml

multiMLton: mandelbrot-sml-par.sml
	../../../build/bin/mlton mandelbrot-sml-par.sml

ghc: mandelbrot.ghc-2.hs
	ghc --make -O2 -threaded -XBangPatterns -optc-mfpmath=sse -optc-msse2 -rtsopts  mandelbrot.ghc-2.hs -o mandelbrot.ghc-2.ghc_run

java:
	javac mandelbrot.java

clean:
	rm -f mandelbrot-sml-seq mandelbrot-sml-par mandelbrot.ghc-2.ghc_run *.o *~ *.class *.hi
