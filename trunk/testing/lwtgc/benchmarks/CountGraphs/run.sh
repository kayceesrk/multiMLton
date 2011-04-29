#!/bin/bash

for i in 1 2 4 6 8 10 12 14 16
do
	./count-graphs-amd64 @MLton number-processors ${i} enable-timer 20000 gc-summary cumulative -- && mv gc-summary.cumul.out gc-summary.cumul.out.${i}
done
