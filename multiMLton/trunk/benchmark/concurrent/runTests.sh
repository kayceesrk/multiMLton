#!/bin/bash
NUMBERS="1 2 4 8 12 16"

for procs in $NUMBERS
do for item in $(ls *-amd64)
   do
   echo "Testing $item on $procs procs"
   ./$item @MLton number-processors $procs enable-timer 10000 -- 10000
   done
done