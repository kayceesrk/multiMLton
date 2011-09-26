SHELL=sh
CLASS=U
NPROCS=1

RCCEROOT=../..
include $(RCCEROOT)/common/symbols

default: header
	@ sys/print_instructions

bt: clean header
	cd sys; make
	cd BT; $(MAKE) \
               CFLAGS="$(CFLAGS)"           \
               NPROCS=$(NPROCS)             \
               CLASS=$(CLASS)               \
               CCOMPILE=$(CCOMPILE)         \
               RCCEROOT=$(RCCEROOT)         \
               RCCEINCLUDE=$(RCCEINCLUDE)   \
               RCK_LIB_SRC=$(RCK_LIB_SRC)   \
               RCCE_LIB_SRC=$(RCCE_LIB_SRC) \
               ARCHIVE=$(ARCHIVE)
               
lu: clean header
	cd sys; make
	cd LU; $(MAKE) \
               CFLAGS="$(CFLAGS)"           \
               NPROCS=$(NPROCS)             \
               CLASS=$(CLASS)               \
               CCOMPILE=$(CCOMPILE)         \
               RCCEROOT=$(RCCEROOT)         \
               RCCEINCLUDE=$(RCCEINCLUDE)   \
               RCK_LIB_SRC=$(RCK_LIB_SRC)   \
               RCCE_LIB_SRC=$(RCCE_LIB_SRC) \
               ARCHIVE=$(ARCHIVE)
               


# It would be nice to make clean in each subdirectory (the targets
# are defined) but on a really clean system this will won't work
# because those makefiles need config/make.def
clean:
	- rm -f core 
	- rm -f *~ */core */*~ */*.o */npbparams.h */*.obj */*.exe
	- rm -f sys/setparams sys/makesuite sys/setparams.h
	- rm -f BT/bt.*.* LU/lu.*.*

header:
	@ sys/print_header

