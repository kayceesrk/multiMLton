all: mlton multiMLton ghc

mlton: mandelbrot-sml-seq.sml
	mlton mandelbrot-sml-seq.sml

multiMLton: mandelbrot-sml-par.sml
	../../../build/bin/mlton mandelbrot-sml-par.sml

ghc: mandelbrot.ghc-2.hs
	ghc --make -O2 -threaded -XBangPatterns -optc-mfpmath=sse -optc-msse2 -rtsopts  mandelbrot.ghc-2.hs -o mandelbrot.ghc-2.ghc_run

clean:
	rm -f mandelbrot-sml-seq mandelbrot-sml-par mandelbrot.ghc-2.ghc_run *.o *~
