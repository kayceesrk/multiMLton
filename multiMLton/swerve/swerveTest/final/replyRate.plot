set term postscript eps enhanced "Helvetica" 20
set output "replyRate.eps"
set title "Reply rate"
set key left top
set xlabel "Connection rate (/s)"
set ylabel "Reply rate (/s)"
plot "PCML_index_1000conn_0iot_6.tsv" using 1:5 with lp title "PCML", \
     "AcmlNB_index_1000conn_3iot_6.tsv" using 1:5 with lp title "ACML", \
     "Mongrel_index_1000conn_iot_4.tsv" using 1:5 with lp title "Mongrel", \
     "Apache_index_1000conn_iot_1.tsv" using 1:5 with lp title "Apache"
