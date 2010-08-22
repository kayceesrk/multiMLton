#!/bin/bash

file="index.html"
d="NB_index"
smallsleep=2
bigsleep=20
numConns=1500
myport=8083
iot=2

resdir="${d}_${numConns}conn_${iot}iot"
rm -r ${resdir}
mkdir ${resdir}
killall -9 main

for proc in 4
do
    directory="${resdir}_${proc}"
    rm -r ${directory}
    mkdir ${directory}

    touch ./${directory}/COLLECT.txt

    echo "Creating swerve for proc = ${proc}"

    echo "hello" > ${HOME}/multiMLton/swervePcmlNB/www/var/test
    rm ${HOME}/multiMLton/swervePcmlNB/www/var/*
    ${HOME}/multiMLton/swervePcmlNB/main/main @MLton number-processors ${proc} enable-timer 10000 io-threads ${iot} fixed-heap 1G -- -f ${HOME}/multiMLton/swervePcmlNB/www/swerve.cfg &
    sleep ${smallsleep}

    echo "Running httperf..."

    for x in 1000 1000 1000 1000 1000
    do
        echo "rate :: ${x}    directory :: ${directory}"

        httperf --port=${myport} --server=e --rate=${x} --num-conns=${numConns} --num-calls=1 --uri=/${file} --timeout=10 > ./${directory}/session_rate${x}.txt

        echo "HTTPERF RATE ${x} :::" >> ./${directory}/COLLECT.txt

        tail -28 ./${directory}/session_rate${x}.txt >> ./${directory}/COLLECT.txt

        echo "Proc :: ${proc}, Rate :: ${x},Going to sleep.. zzzzzz..."
        sleep ${bigsleep}
        echo "Woke up!!"

    done
    killall -9 main
    perl parse.pl ${directory}/COLLECT.txt ${numConns} > ${resdir}/${directory}.tsv
    rm -r ${directory}
    sleep ${bigsleep}

done
