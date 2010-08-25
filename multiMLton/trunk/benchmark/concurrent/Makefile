#!/bin/bash

DEBUG := false
PROFILE := no
STACK := false
MULTIMLTON=$(HOME)/git/multiMLton/trunk/build/bin/mlton

SRCS := $(wildcard *.sml)
SRC_RULS := $(subst .sml,-amd64,$(SRCS))

%-amd64 : $(SRCS)
	$(MULTIMLTON) -debug $(DEBUG) -profile $(PROFILE) -profile-stack $(STACK) -output $@ "$(subst -amd64,,$@).sml"

all: $(SRC_RULS)

clean:
	rm -rf $(SRC_RULS) *~ op *.out
