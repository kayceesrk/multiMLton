SHELL=sh
RCCEROOT=../..
include $(RCCEROOT)/common/symbols

HELLOOBJS=RCCE_hello.o  $(ARCHIVE)

default:
	@echo "Usage: make hello "
	@echo "       make clean"

hello: $(HELLOOBJS)
	$(CCOMPILE) -o hello $(HELLOOBJS) $(CFLAGS)


RCCE_hello.o: RCCE_hello.c $(RCCEINCLUDE)/RCCE.h
	$(CCOMPILE) -c $(CFLAGS) RCCE_hello.c  

clean:
	@ rm -f *.o  hello 
