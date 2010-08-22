set term postscript eps enhanced "Helvetica" 20
set output "replyRate.eps"
set title "Swerve reply rate - 851K 400Cons 1IOT"
set key left top
set xlabel "Connection rate (/s)"
set ylabel "Reply rate (/s)"
plot "NB_851K_400conn_1iot_1.tsv" using 1:5 with lp title "1 proc", \
     "NB_851K_400conn_1iot_2.tsv" using 1:5 with lp title "2 proc", \
     "NB_851K_400conn_1iot_4.tsv" using 1:5 with lp title "4 proc", \
     "NB_851K_400conn_1iot_6.tsv" using 1:5 with lp title "6 proc", \
     "NB_851K_400conn_1iot_8.tsv" using 1:5 with lp title "8 proc", \
     "NB_851K_400conn_1iot_10.tsv" using 1:5 with lp title "10 proc", \
     "NB_851K_400conn_1iot_12.tsv" using 1:5 with lp title "12 proc"
