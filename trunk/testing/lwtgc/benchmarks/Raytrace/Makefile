#!/bin/bash

DEBUG := false
PROFILE := no
STACK := false
BRANCH := false
MORE :=

SRCS := $(wildcard *.sml)
SRC_RULS := $(subst .sml,-amd64,$(SRCS))

MULTIMLTON=../../../../build/bin/mlton

%-amd64 : $(SRCS)
	$(MULTIMLTON) $(MORE) -keep g -debug $(DEBUG) -profile $(PROFILE) -profile-stack $(STACK) -profile-branch $(BRANCH) -output $@ "$(subst -amd64,,$@).sml"

all: $(SRC_RULS)

clean:
	rm -rf $(SRC_RULS) *~ op *.out *.c *ssa *ssa2 *xml *-ml *machine

clean-summaries:
	rm gc-summary.*

MMPATH = $(HOME)/temp/multiMLton

FLAGS = -std=gnu99 -c -g -DASSERT=1 -I$(MMPATH)/trunk/build/lib/self/include \
				-I$(MMPATH)/trunk/build/lib/include -O1 -fno-common -fno-strict-aliasing \
				-fomit-frame-pointer -w -m64

DEBUG_LIBS = -L$(MMPATH)/trunk/build/lib/self -lmlton-gdb -lgdtoa-gdb \
			 -lm -lgmp -m64 -lplpa_included -pthread -lrt

LIBS = -L$(MMPATH)/trunk/build/lib/self -lmlton -lgdtoa \
			 -lm -lgmp -m64 -lplpa_included -pthread -lrt

%.o: %.c
	gcc $(FLAGS) -o $@ $<

.SECONDEXPANSION:
%-cnd : $$(subst .c,.o,$$(wildcard $$(subst -cnd,,$$@).*.c))
	gcc -o $(subst -c,,$@) $^ $(LIBS)


.SECONDEXPANSION:
%-cdebug : $$(subst .c,.o,$$(wildcard $$(subst -cdebug,,$$@).*.c))
	gcc -o $(subst -cdebug,,$@) $^ $(DEBUG_LIBS)
