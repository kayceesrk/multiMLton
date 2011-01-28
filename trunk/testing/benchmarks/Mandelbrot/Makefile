#!/bin/bash

DEBUG := false
PROFILE := no
STACK := false
BRANCH := false
MORE :=
MULTIMLTON=$(HOME)/temp/UT/multiMLton/trunk/build/bin/mlton

SRCS := $(wildcard *.sml)
SRC_RULS := $(subst .sml,-amd64,$(SRCS))

%-amd64 : $(SRCS)
	$(MULTIMLTON) $(MORE) -keep g -debug $(DEBUG) -profile $(PROFILE) -profile-stack $(STACK) -profile-branch $(BRANCH) -output $@ "$(subst -amd64,,$@).sml"

all: $(SRC_RULS)

clean:
	rm -rf $(SRC_RULS) *~ op *.out *.c *ssa *ssa2 *xml *-ml *machine
