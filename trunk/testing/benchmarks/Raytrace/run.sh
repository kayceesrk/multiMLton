#!/bin/bash

for i in 1 2 4 6 8 10 12 14 16
do
	  ./${1} @MLton number-processors ${i} enable-timer 20000 gc-summary -- ${2} &> gc-summary.cumul.out.${i}
done
