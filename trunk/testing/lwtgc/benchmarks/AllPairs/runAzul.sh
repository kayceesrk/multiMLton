#!/bin/bash

FILENAME=$1
GDB=$2
NUM_PROCS=$3

ARGS="512 32"

echo running $FILENAME

if [ $GDB -eq 1  ]; then
~/azul/_install/azul/avm-purdue/sandbox/linux/bin/i686/cproxy2 -h 128.10.129.75 -PX:+StartSuspended $FILENAME @MLton number-processors $NUM_PROCS -- $ARGS
else
~/azul/_install/azul/avm-purdue/sandbox/linux/bin/i686/cproxy2 -h 128.10.129.75 -PX:MemCommit=8g \
	$FILENAME @MLton number-processors $NUM_PROCS gc-summary cumulative -- $ARGS
fi
