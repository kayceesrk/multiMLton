#!/bin/bash

file="index.html"
d="PCML_index"
smallsleep=3
bigsleep=3
numConns=1000
myport=8082
iot=0

resdir="${d}_${numConns}conn_${iot}iot"
rm -r ${resdir}
mkdir ${resdir}
killall -9 main

for proc in 6
do
    directory="${resdir}_${proc}"
    rm -r ${directory}
    mkdir ${directory}

    touch ./${directory}/COLLECT.txt

    echo "Creating swerve for proc = ${proc}"

    echo "hello" > ${HOME}/multiMLton/swervePcml/www/var/test
    rm ${HOME}/multiMLton/swervePcml/www/var/*
    ${HOME}/multiMLton/swervePcml/main/main @MLton number-processors ${proc} enable-timer 1000 io-threads ${iot} fixed-heap 500M -- -f ${HOME}/multiMLton/swervePcml/www/swerve.cfg &
    sleep ${smallsleep}

    echo "Running httperf..."

    for x in 1000 1100 1200 1400 1600 1800 2000
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
