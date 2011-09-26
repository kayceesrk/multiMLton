SHELL=sh
RCCEROOT=../..
include $(RCCEROOT)/common/symbols

SHAREOBJS=RCCE_share.o $(ARCHIVE)

default:
	@echo "Usage: make share"
	@echo "       make clean"


share: $(SHAREOBJS)
	$(CCOMPILE) -o share $(SHAREOBJS) $(CFLAGS)


RCCE_share.o: RCCE_share.c $(RCCEINCLUDE)/RCCE.h
	$(CCOMPILE) -c $(CFLAGS) RCCE_share.c  


clean:
	@ rm -f *.o  share 




