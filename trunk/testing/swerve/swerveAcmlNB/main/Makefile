#!/bin/bash

CFLAGS = -std=gnu99 -g -DASSERT=1 -fno-strict-aliasing -fomit-frame-pointer -w -m64
INCLUDES = -I/home/chandras/git/trunk/build/lib/self/include \
					 -I/home/chandras/git/trunk/build/lib/include
FLAGS = -std=gnu99 -c -g -DASSERT=1 -I/home/chandras/git/trunk/build/lib/self/include \
				-I/home/chandras/git/trunk/build/lib/include -O1 -fno-common -fno-strict-aliasing \
				-fomit-frame-pointer -w -m64

DEBUG_LIBS = -L/home/chandras/git/trunk/build/lib/self -lmlton-gdb -lgdtoa-gdb \
			 -lm -lgmp -m64 -lplpa_included -pthread -lrt

LIBS = -L/home/chandras/git/trunk/build/lib/self -lmlton -lgdtoa \
			 -lm -lgmp -m64 -lplpa_included -pthread -lrt

SRC := $(wildcard *.c)

OBJ := $(patsubst %.c,%.o,$(wildcard $(SRC)))

.PHONY: all clean
all: $(OBJ)

clean:
	rm -f $(OBJ)

%.o: %.c
	gcc $(FLAGS) -o $@ $<

main: $(OBJ)
	gcc -o $@ $^ $(LIBS)

main-debug: $(OBJ)
	gcc -o main $^ $(DEBUG_LIBS)
