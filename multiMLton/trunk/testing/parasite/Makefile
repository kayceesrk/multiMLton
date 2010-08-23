#!/bin/bash

MULTIMLTON=$(HOME)/git/multiMLton/trunk/build/bin/mlton

CORES=0-15
TASKSET=taskset

SRCS := $(wildcard *.sml)
SRC_RULS := $(subst .sml,-amd64,$(SRCS))

ping-pong%-amd64 : $(SRCS)
	$(MULTIMLTON) -verbose 2 -output $@ "$(subst -amd64,,$@).sml"

all: $(SRC_RULS)

clean:
	rm -rf $(SRC_RULS) *~
