set term postscript eps enhanced "Helvetica" 20
set output "ioRate.eps"
set title "Swerve IO"
set key left top
set xlabel "Connection rate (/s)"
set ylabel "I/O rate (KBps)"
plot "van1.tsv" using 1:11 with lp title "Orig - 1 proc", \
     "van2.tsv" using 1:11 with lp title "Orig - 2 proc", \
     "van4.tsv" using 1:11 with lp title "Orig - 4 proc", \
     "nb1_1io.tsv" using 1:11 with lp title "NB - 1 proc",  \
     "nb2_1io.tsv" using 1:11 with lp title "NB - 2 proc",  \
     "nb4_1io.tsv" using 1:11 with lp title "NB - 4 proc"
