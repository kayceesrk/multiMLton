set term postscript eps enhanced "Helvetica" 20
set output "commCost2.eps"
set title "ping-pong on 2 processors"
set key left top
set xlabel "Iterations"
set ylabel "Time (ms)"
set log xy
plot "asyncCommCost_2Proc.tsv" using 1:2 with lp lw 2 title "sync", \
     "asyncCommCost_2Proc.tsv" using 1:3 with lp lw 2 title "spawn send"
