SHELL=sh
RCCEROOT=../..
include $(RCCEROOT)/common/symbols

STENCILOBJS=RCCE_stencil.o $(ARCHIVE) 
STENCILSYNCHOBJS= RCCE_stencil_synch.o $(ARCHIVE)
PSTENCILOBJS=RCCE_pstencil.o $(ARCHIVE) 
POWERRESETOBJS=RCCE_power_reset.o $(ARCHIVE) 
FVOBJS=RCCE_FV.o $(ARCHIVE) 
FDIVOBJS=RCCE_Fdiv.o $(ARCHIVE) 

default:
	@echo "Usage: make           stencil_synch"
	@echo "       make API=gory  stencil"
	@echo "       make PWRMGMT=1 pstencil"
	@echo "       make PWRMGMT=1 power_reset"
	@echo "       make PWRMGMT=1 Fdiv"
	@echo "       make PWRMGMT=1 FV"
	@echo "       make           clean"

FV: $(FVOBJS)
	$(CCOMPILE) -o FV $(FVOBJS) $(CFLAGS)

Fdiv: $(FDIVOBJS)
	$(CCOMPILE) -o Fdiv $(FDIVOBJS) $(CFLAGS)

stencil: $(STENCILOBJS)
	$(CCOMPILE) -o stencil $(STENCILOBJS) $(CFLAGS)

stencil_synch: $(STENCILSYNCHOBJS)
	$(CCOMPILE)  -o stencil_synch  $(STENCILSYNCHOBJS)   $(CFLAGS)   
#use line below for gcc, which does not link libm by default
#	$(CCOMPILE)  -o stencil_synch  $(STENCILSYNCHOBJS)   $(CFLAGS)    -lm

pstencil: $(PSTENCILOBJS)
	$(CCOMPILE) -o pstencil $(PSTENCILOBJS) $(CFLAGS)

power_reset: $(POWERRESETOBJS)
	$(CCOMPILE) -o power_reset $(POWERRESETOBJS) $(CFLAGS)

RCCE_FV.o: RCCE_FV.c $(RCCEINCLUDE)/RCCE.h
	$(CCOMPILE) -c $(CFLAGS) RCCE_FV.c  

RCCE_Fdiv.o: RCCE_Fdiv.c $(RCCEINCLUDE)/RCCE.h
	$(CCOMPILE) -c $(CFLAGS) RCCE_Fdiv.c  

RCCE_stencil.o: RCCE_stencil.c $(RCCEINCLUDE)/RCCE.h
	$(CCOMPILE) -c $(CFLAGS) RCCE_stencil.c  

RCCE_stencil_synch.o: RCCE_stencil_synch.c $(RCCEINCLUDE)/RCCE.h
	$(CCOMPILE) -c $(CFLAGS)   RCCE_stencil_synch.c  

RCCE_pstencil.o: RCCE_pstencil.c $(RCCEINCLUDE)/RCCE.h
	$(CCOMPILE) -c $(CFLAGS) RCCE_pstencil.c  

RCCE_power_reset.o: RCCE_power_reset.c $(RCCEINCLUDE)/RCCE.h
	$(CCOMPILE) -c $(CFLAGS) RCCE_power_reset.c  

clean:
	@ rm -f *.o power_reset  stencil pstencil FV Fdiv stencil_synch  __*__* linker.map *.txt *.s



