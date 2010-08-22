#!/bin/bash

directory=$1

rm -r ${directory}
mkdir ${directory}

touch ./${directory}/COLLECT.txt

for x in 10.0 20.0 30.0 40.0 50.0 60.0 70.0 80.0 90.0 100.0 120.0 140.0 160.0 180.0 200.0
do
    echo "rate :: ${x}    directory :: ${directory}"

    httperf --port=8083 --server=e --rate=${x} --num-conns=400 --num-calls=1 --uri=/primes-multicastM --timeout=100 > ./${directory}/session_rate${x}.txt

    echo "HTTPERF RATE ${x} :::" >> ./${directory}/COLLECT.txt

    tail -28 ./${directory}/session_rate${x}.txt >> ./${directory}/COLLECT.txt

    echo "Going to sleep.. zzzzzz..."
    sleep 10
    echo "Woke up!!"

done
