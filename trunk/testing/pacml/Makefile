#!/bin/bash

DEBUG := false
PROFILE := no
STACK := false
BRANCH := false
MORE :=

SRCS := $(wildcard *.sml)
SRC_RULS := $(subst .sml,-amd64,$(SRCS))

MMPATH = $(HOME)/multiMLton-RB
MULTIMLTON= $(MMPATH)/trunk/build/bin/mlton

%-scc : $(SRCS)
	$(MULTIMLTON) $(MORE) -keep g -debug $(DEBUG) -profile $(PROFILE) -profile-stack $(STACK) -profile-branch $(BRANCH) -output $@ "$(subst -scc,,$@).sml"

all: $(SRC_RULS)

clean:
	rm -rf $(SRC_RULS) *~ op *.out *.c *ssa *ssa2 *xml *-ml *machine


FLAGS = -std=gnu99 -c -g -DASSERT=1 -I$(MMPATH)/trunk/build/lib/self/include \
	    -I$(MMPATH)/trunk/build/lib/include -O1 -fno-common -fno-strict-aliasing \
	    -fomit-frame-pointer -w -I/shared/chandras/rcce/include -m32 -fno-strength-reduce \
        -fschedule-insns -fschedule-insns2 -malign-functions=5 -malign-jumps=2 -malign-loops=2

DEBUG_LIBS = -g -L$(MMPATH)/trunk/build/lib/self -lmlton-gdb -lgdtoa-gdb \
             -L/shared/chandras/rcce/lib -L/shared/chandras/gmp5/lib -lm \
             -lgmp -lrcce -static -lplpa_included -pthread -lrt -m32

LIBS = -L$(MMPATH)/trunk/build/lib/self -lmlton -lgdtoa -L/shared/chandras/rcce/lib \
       -L/shared/chandras/gmp5/lib -lm -lgmp -lrcce -static -lplpa_included \
       -pthread -lrt -m32

%.o: %.c
	gcc $(FLAGS) -o $@ $<

.SECONDEXPANSION:
%-c : $$(subst .c,.o,$$(wildcard $$(subst -c,,$$@).*.c))
	gcc -o $(subst -c,,$@) $^ $(LIBS)


.SECONDEXPANSION:
%-cdebug : $$(subst .c,.o,$$(wildcard $$(subst -cdebug,,$$@).*.c))
	gcc -o $(subst -cdebug,,$@) $^ $(DEBUG_LIBS)
