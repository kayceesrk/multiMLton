SHELL=sh

RCCEROOT=../..
include $(RCCEROOT)/common/symbols

ECOQOBJS=RCCE_eco_q.o RCCE_pwr_wq_framework.o $(ARCHIVE)

default:
	@echo Usage: make PWRMGMT=1 eco_q [clean]

eco_q: $(ECOQOBJS)
	$(CCOMPILE) -o eco_q $(ECOQOBJS) $(CFLAGS)

RCCE_eco_q.o: RCCE_eco_q.c $(RCCEINCLUDE)/RCCE.h RCCE_pwr_wq.h 
	$(CCOMPILE) -c $(CFLAGS) RCCE_eco_q.c  

RCCE_pwr_wq_framework.o: RCCE_pwr_wq_framework.c $(RCCEINCLUDE)/RCCE.h RCCE_pwr_wq.h
	$(CCOMPILE) -c $(CFLAGS) RCCE_pwr_wq_framework.c  

clean:
	@ rm -f	*.o wq eco_q FV_reset



