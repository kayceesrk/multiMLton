set term postscript eps enhanced "Helvetica" 20
set output "commCostWithAcml1.eps"
set title "ping-pong with ACML"
set key left top
set xlabel "# Communication actions"
set ylabel "Time (ms)"
set log xy
plot "asyncCommCost_1Proc.tsv" using 1:2 with lp lw 2 title "sync", \
     "asyncCommCost_1Proc.tsv" using 1:3 with lp lw 2 title "spawn send", \
     "asyncCommCost_1Proc.tsv" using 1:4 with lp lw 2 title "ACML"
