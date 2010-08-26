#!/bin/bash
NUMBERS="1 2 4 6 8 10 12 14 16"

for procs in $NUMBERS
do for item in $(ls *-amd64)
   do
   echo "Testing $item on $procs procs"
   ./$item @MLton number-processors $procs enable-timer 20000 -- 10000
   done
done