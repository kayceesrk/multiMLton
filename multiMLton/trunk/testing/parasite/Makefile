#!/bin/bash

MULTIMLTON=$(HOME)/git/multiMLton/trunk/build/bin/mlton
BASIS_DIR=$(HOME)/git/multiMLton/trunk/build/lib/sml/basis

CORES=0-15
TASKSET=taskset

SRCS := $(wildcard *.sml)
SRC_RULS := $(subst .sml,-amd64,$(SRCS))

%-amd64 : $(SRCS)
	$(MULTIMLTON) -output $@ "$(subst -amd64,,$@).sml"

all: $(SRC_RULS)

clean:
	rm -rf $(SRC_RULS) *~ op
