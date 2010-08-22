set term postscript eps enhanced "Helvetica" 20
set output "replyRate.eps"
set title "Swerve reply rate - 851K 400Cons 1IOT"
set key left top
set xlabel "Connection rate (/s)"
set ylabel "Reply rate (/s)"
plot "NB_851K_400Conns_1iot1.tsv" using 1:5 with lp title "1 proc", \
     "NB_851K_400Conns_1iot2.tsv" using 1:5 with lp title "2 proc", \
     "NB_851K_400Conns_1iot4.tsv" using 1:5 with lp title "4 proc", \
     "NB_851K_400Conns_1iot6.tsv" using 1:5 with lp title "4 proc", \
     "NB_851K_400Conns_1iot8.tsv" using 1:5 with lp title "4 proc", \
     "NB_851K_400Conns_1iot10.tsv" using 1:5 with lp title "10 proc", \
     "NB_851K_400Conns_1iot12.tsv" using 1:5 with lp title "12 proc",
