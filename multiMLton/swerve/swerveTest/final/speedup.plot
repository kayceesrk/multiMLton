set term postscript eps enhanced "Helvetica" 20
set output "speedup.eps"
set title "Swerve speedup of ACML over PCML"
set key left top
set xlabel "Connection rate (/s)"
set ylabel "Speedup (X times)"
plot "speedup1.tsv" using 1:2 with lp title "1 proc", \
     "speedup2.tsv" using 1:2 with lp title "2 proc", \
     "speedup4.tsv" using 1:2 with lp title "4 proc"
