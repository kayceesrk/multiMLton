all: mlton multiMLton ghc java c

mlton: spectralnorm-seq.sml
	mlton spectralnorm-seq.sml

multiMLton: spectralnorm-par.sml
	../../../build/bin/mlton spectralnorm-par.sml

ghc: spectralnorm.ghc-4.hs
	ghc --make -O2 -threaded -funbox-strict-fields -O2 -fvia-c -optc-O3 -fexcess-precision -optc-ffast-math -XBangPatterns -XMagicHash -rtsopts  spectralnorm.ghc-4.hs -o spectralnorm.ghc-4.ghc_run

java:
	javac spectralnorm.java

c:
	gcc -pipe -Wall -O3 -fomit-frame-pointer -march=native -fopenmp -mfpmath=sse -msse2 spectralnorm.gcc-5.c -o spectralnorm.gcc-5.gcc_run -lm

clean:
	rm -f spectralnorm-seq spectralnorm-par *.ghc_run *.o *~ *.class *.hi *gcc_run
