set term postscript eps enhanced "Helvetica" 20
set output "replyRate.eps"
set title "Swerve reply rate"
set key left top
set xlabel "Connection rate (/s)"
set ylabel "Reply rate (/s)"
plot "van1.tsv" using 1:5 with lp title "Orig - 1 proc", \
     "van2.tsv" using 1:5 with lp title "Orig - 2 proc", \
     "van4.tsv" using 1:5 with lp title "Orig - 4 proc", \
     "nb1_1io.tsv" using 1:5 with lp title "NB - 1 proc",  \
     "nb2_1io.tsv" using 1:5 with lp title "NB - 2 proc",  \
     "nb4_1io.tsv" using 1:5 with lp title "NB - 4 proc"
