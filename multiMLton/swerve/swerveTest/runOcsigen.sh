#!/bin/bash

d="Ocsigen_index"
smallsleep=1
bigsleep=1
numConns=1000
myport=8083

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

    for x in 100.0 120.0 140.0 160.0 180.0 200.0 250.0 300.0 350.0 400.0 500.0 600.0 700.0 800.0 900.0 1000.0
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
