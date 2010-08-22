#!/bin/bash

d="Apache_index"
smallsleep=1
bigsleep=1
numConns=1000
myport=8084

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


    echo "Running httperf..."

    for x in 100 200.0 300 400.0 500 600.0 700 800.0 900 1000.0 1000.0 1100 1200.0 1300 1400.0 1500 1600.0 1700 1800.0 1900 2000.0
    do
        echo "rate :: ${x}    directory :: ${directory}"

        httperf --port=${myport} --server=e --rate=${x} --num-conns=${numConns} --num-calls=1 --uri=/index.html --timeout=10 > ./${directory}/session_rate${x}.txt

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
