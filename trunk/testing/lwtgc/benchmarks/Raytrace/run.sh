#!/bin/bash

for i in 1 2 4 6 8 10 12 14 16
do
	./${1} @MLton number-processors ${i} gc-summary cumulative -- ${2} && mv gc-summary.cumul.out gc-summary.cumul.out.${i}
done
