SHELL=sh
RCCEROOT=../..
include $(RCCEROOT)/common/symbols

SHIFTOBJS=RCCE_shift.o $(ARCHIVE)
CSIMSHIFTOBJS=RCCE_simshift.o $(ARCHIVE)

default:
	@echo "Usage: make csimshift"
	@echo "       make API=gory cshift"
	@echo "       make clean"


recvtest: $(RECVTESTOBJS)
	$(CCOMPILE) -o recvtest $(RECVTESTOBJS) $(CFLAGS)

cshift: $(SHIFTOBJS)
	$(CCOMPILE) -o cshift $(SHIFTOBJS) $(CFLAGS)

csimshift: $(CSIMSHIFTOBJS)
	$(CCOMPILE) -o csimshift $(CSIMSHIFTOBJS) $(CFLAGS)

RCCE_shift.o: RCCE_shift.c $(RCCEINCLUDE)/RCCE.h
	$(CCOMPILE) -c $(CFLAGS) RCCE_shift.c  

RCCE_simshift.o: RCCE_simshift.c $(RCCEINCLUDE)/RCCE.h
	$(CCOMPILE) -c $(CFLAGS) RCCE_simshift.c  


clean:
	@ rm -f *.o  cshift csimshift 




