#!/bin/bash

DEBUG := false
PROFILE := no
STACK := false
BRANCH := false
MORE :=

SRCS := $(wildcard *.sml)
SRC_RULS := $(subst .sml,-azul,$(SRCS))

MULTIMLTON=../../../../build/bin/mlton

MMPATH = $(HOME)/multiMLton-WB

FLAGS = -std=gnu99 -ggdb -c -I $(AZUL_MULTIMLTON_RUNTIME) -I $(AZUL_MULTIMLTON_INCLUDE) \
				-O1 -fno-common -fno-strict-aliasing -I ~/azul/_install/azul/avm-purdue/sandbox/aztek/include \
				-fomit-frame-pointer

LIBS = -L $(AZUL_MULTIMLTON_RUNTIME) -lmlton -lgdtoa -lm \
			 -lgmp -Xlinker --start-group ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libazprof_g.a \
			 ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libcrypto_g.a ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libregex_g.a \
			 ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libssl_g.a ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libproxy_g.a \
			 ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/librpc_g.a ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libfdc_g.a \
			 ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libazulversion_g.a ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libtcpip_g.a \
			 ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libtransport_g.a ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libmon_g.a \
			 ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libazpr_g.a ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libos.a \
			 ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libos_g.a -lstdc++ -lm -lgcc -lm -lgmp  \
			 ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libc.a /home/chandras/azul/_install/swrelease/swtools/1.04.0428/cross/lib/libm.a \
			 -Xlinker --end-group

DEBUG_LIBS = -L $(AZUL_MULTIMLTON_RUNTIME) -lmlton-gdb -lgdtoa-gdb -lm \
			 -lgmp -Xlinker --start-group ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libazprof_g.a \
			 ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libcrypto_g.a ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libregex_g.a \
			 ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libssl_g.a ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libproxy_g.a \
			 ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/librpc_g.a ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libfdc_g.a \
			 ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libazulversion_g.a ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libtcpip_g.a \
			 ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libtransport_g.a ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libmon_g.a \
			 ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libazpr_g.a ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libos.a \
			 ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libos_g.a -lstdc++ -lm -lgcc -lm -lgmp  \
			 ~/azul/_install/azul/avm-purdue/sandbox/aztek/lib/vega2/libc.a /home/chandras/azul/_install/swrelease/swtools/1.04.0428/cross/lib/libm.a \
			 -Xlinker --end-group

ifeq ($(DEBUG), true)
	LIBS := DEBUG_LIBS
endif


%.o: %.c
	@azgcc $(FLAGS) -o $@ $<


.SECONDEXPANSION:
%-cnd : $$(subst .c,.o,$$(wildcard $$(subst -cnd,,$$@).*.c))
	@azg++ -o $(subst -cnd,,$@) $^ $(LIBS)

.SECONDEXPANSION:
%-cdebug : $$(subst .c,.o,$$(wildcard $$(subst -cdebug,,$$@).*.c))
	azg++ -o $(subst -cdebug,,$@) $^ $(DEBUG_LIBS)

%-azul: $(SRCS)
	$(MULTIMLTON) $(MORE) -align 8 -keep g -debug $(DEBUG) -profile $(PROFILE) -profile-stack $(STACK) -profile-branch $(BRANCH) -output $@ -stop g "$(subst -azul,,$@).sml"

clean:
	rm -rf $(SRC_RULS) *~ op *.out *.c *ssa *ssa2 *xml *-ml *machine

clean-summaries:
	rm gc-summary.*
