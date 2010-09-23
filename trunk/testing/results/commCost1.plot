set term postscript eps enhanced "Helvetica" 20
set output "commCost1.eps"
set title "ping-pong"
set key left top
set xlabel "# Communication actions"
set ylabel "Time (ms)"
set log xy
plot "asyncCommCost_1Proc.tsv" using 1:2 with lp lw 2 title "sync", \
     "asyncCommCost_1Proc.tsv" using 1:3 with lp lw 2 title "spawn send"
