all: mlton multiMLton ghc java c fasta

.PHONY: mlton multiMLton ghc java c fasta

mlton: knucleotide-seq.mlb knucleotide-seq.sml
	mlton knucleotide-seq.mlb

multiMLton: knucleotide-par.sml knucleotide-par.mlb
	../../../build/bin/mlton knucleotide-par.mlb

ghc: knucleotide.ghc-2.hs
	ghc --make -O2 -XBangPatterns -threaded -rtsopts -funbox-strict-fields knucleotide.ghc-2.hs -o knucleotide.ghc-2.ghc_run

java:
	javac knucleotide.java

c:
	g++ -c -pipe -O3 -fomit-frame-pointer -march=native  -std=c++0x knucleotide.gpp-3.c++ -o knucleotide.gpp-3.c++.o &&  \
		        /usr/bin/g++ knucleotide.gpp-3.c++.o -o knucleotide.gpp-3.gpp_run -Wl,--no-as-needed -lpthread

fasta:
	make -C fasta all

clean:
	rm -f knucleotide-seq knucleotide-par *.ghc_run *.o *~ *.class *.hi
	make -C fasta clean
