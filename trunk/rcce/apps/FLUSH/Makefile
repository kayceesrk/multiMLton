SHELL=sh
RCCEROOT=../..
include $(RCCEROOT)/common/symbols

CACHEABLEOBJS=RCCE_test_cacheable.o  $(ARCHIVE)

default:
	@echo "Usage: make test_cacheable "
	@echo "       make clean"

test_cacheable: $(CACHEABLEOBJS)
	$(CCOMPILE) -o test_cacheable $(CACHEABLEOBJS) $(CFLAGS)


RCCE_test_cacheable.o: RCCE_test_cacheable.c $(RCCEINCLUDE)/RCCE.h Makefile
	$(CCOMPILE) -c $(CFLAGS) RCCE_test_cacheable.c  

clean:
	@ rm -f *.o  test_cacheable 
