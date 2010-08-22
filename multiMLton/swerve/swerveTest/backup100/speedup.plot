set term postscript eps enhanced "Helvetica" 20
set output "speedup.eps"
set title "Swerve speedup"
set key left top
set xlabel "Connection rate (/s)"
set ylabel "Speedup (X times)"
plot "speedup.dat" using 1:2 with lp title "1 proc", \
     "speedup.dat" using 1:3 with lp title "2 proc", \
     "speedup.dat" using 1:4 with lp title "4 proc"
