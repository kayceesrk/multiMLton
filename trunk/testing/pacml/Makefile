#!/bin/bash

DEBUG := false
PROFILE := no
STACK := false
BRANCH := false
KEEPC := false
MORE :=

SRCS := $(wildcard *.sml)
SRC_RULS := $(subst .sml,-amd64,$(SRCS))

MULTIMLTON := ../../build/bin/mlton

%-amd64 : $(SRCS)
	$(MULTIMLTON) $(MORE) -debug $(DEBUG) -profile $(PROFILE) -profile-stack $(STACK) -profile-branch $(BRANCH) -output $@ "$(subst -amd64,,$@).sml"

all: $(SRC_RULS)

clean:
	rm -rf $(SRC_RULS) *~ op *.out *.c *.ssa *.rssa *xml *-ml *ssa2 *machine
