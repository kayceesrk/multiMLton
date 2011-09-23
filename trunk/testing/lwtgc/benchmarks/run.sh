#!/bin/bash

source ~/.bashrc
python findHeapValues.py -b $1
python runBench.py -b $1
python analyze.py -b $1
