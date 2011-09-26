SHELL=sh
RCCEROOT=../..
include $(RCCEROOT)/common/symbols

PINGPONGOBJS=RCCE_pingpong.o  $(ARCHIVE)
PINGPONGXTOBJS=RCCE_pingpong-xt.o  $(ARCHIVE)

default:
	@echo "Usage: make pingpong "
	@echo "       make pingpong-xt "
	@echo "       make clean"

pingpong: $(PINGPONGOBJS)
	$(CCOMPILE) -o pingpong $(PINGPONGOBJS) $(CFLAGS)

pingpong-xt: $(PINGPONGXTOBJS)
	$(CCOMPILE) -o pingpong-xt $(PINGPONGXTOBJS) $(CFLAGS)

RCCE_pingpong.o: RCCE_pingpong.c $(RCCEINCLUDE)/RCCE.h
	$(CCOMPILE) -c $(CFLAGS) RCCE_pingpong.c  

RCCE_pingpong-xt.o: RCCE_pingpong-xt.c $(RCCEINCLUDE)/RCCE.h
	$(CCOMPILE) -c $(CFLAGS) RCCE_pingpong-xt.c  

clean:
	@ rm -f *.o  pingpong pingpong-xt
