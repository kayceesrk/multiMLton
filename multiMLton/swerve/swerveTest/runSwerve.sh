#!/bin/bash

file="primes_multicast"
d="SingleCore_primes"
smallsleep=5
bigsleep=10
numConns=1000
myport=9000
iot=2

resdir="${d}_${numConns}conn_${iot}iot"
rm -r ${resdir}
mkdir ${resdir}

for proc in 1
do
    directory="${resdir}_${proc}"
    rm -r ${directory}
    mkdir ${directory}

    touch ./${directory}/COLLECT.txt

    echo "Creating swerve for proc = ${proc}"

    echo "hello" > ${HOME}/swerve/www/var/test
    rm ${HOME}/swerve/www/var/*
    ${HOME}/swerve/main/main -f ${HOME}/swerve/www/swerve.cfg &
    sleep ${smallsleep}

    echo "Running httperf..."

    for x in 100 200 300 400 500 600 700 800 900.0 1000.0 1200.0
    do
        echo "rate :: ${x}    directory :: ${directory}"

        httperf --port=${myport} --server=e --rate=${x} --num-conns=${numConns} --num-calls=1 --uri=/${file} --timeout=10 > ./${directory}/session_rate${x}.txt

        echo "HTTPERF RATE ${x} :::" >> ./${directory}/COLLECT.txt

        tail -28 ./${directory}/session_rate${x}.txt >> ./${directory}/COLLECT.txt

        echo "Proc :: ${proc}, Rate :: ${x},Going to sleep.. zzzzzz..."
        sleep ${bigsleep}
        echo "Woke up!!"

    done
    pid=`ps -a | grep "main" | cut -d' ' -f1`
    echo "Killing swerve with pid ${pid}"
    kill -9 ${pid}
    perl parse.pl ${directory}/COLLECT.txt ${numConns} > ${resdir}/${directory}.tsv
    rm -r ${directory}
    sleep ${bigsleep}

done
