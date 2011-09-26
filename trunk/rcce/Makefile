include common/symbols

ifeq ($(OMP_EMULATOR),0)
  PLATFORMOBJS=SCC_API.o   
else
  PLATFORMOBJS=RCCE_emulator_driver.o
endif

ifeq ($(PWRMGMT),1)
  POWEROBJS=RCCE_power_management.o
endif

ARCHIVEOBJS= RCCE_admin.o RCCE_comm.o   RCCE_malloc.o RCCE_qsort.o RCCE_synch.o RCCE_flags.o  \
             RCCE_send.o  RCCE_recv.o   RCCE_debug.o  RCCE_get.o   RCCE_put.o   RCCE_reduce.o \
             RCCE_bcast.o RCCE_shmalloc.o RCCE_DCMflush.o $(PLATFORMOBJS) $(POWEROBJS)

ifeq ($(OMP_EMULATOR),0)
	ARCHIVEOBJS += RCCE_memcpy.o
endif

$(ARCHIVE): $(ARCHIVEOBJS)
	@echo Archive name = $(ARCHIVE) 
	ar -r $(ARCHIVE) $(ARCHIVEOBJS) 
	rm -f *.o

usage:
	@echo "         make [OMP_EMULATOR=0] [PWRMGMT=1] [API=gory]  [SINGLEBITFLAGS=1]"
	@echo "         make [clean] [veryclean]" 
	@echo "default: make  OMP_EMULATOR=1   PWRMGMT=0   API=nongory SINGLEBITFLAGS=0"

RCCE_admin.o: $(RCCE_LIB_SRC)/RCCE_admin.c $(RCCEINCLUDE)/RCCE.h $(RCCEINCLUDE)/RCCE_lib.h \
        $(RCCEINCLUDE)/RCCE_lib_pwr.h
	$(CCOMPILE) -c $(CFLAGS) $(RCCE_LIB_SRC)/RCCE_admin.c  $(RCCE_FLAGS) 

RCCE_power_management.o: $(RCCE_LIB_SRC)/RCCE_power_management.c $(RCCEINCLUDE)/RCCE.h \
         $(RCCEINCLUDE)/RCCE_lib.h $(RCCEINCLUDE)/SCC_API.h $(RCCEINCLUDE)/RCCE_lib_pwr.h
	$(CCOMPILE) -c $(CFLAGS) $(RCCE_LIB_SRC)/RCCE_power_management.c  $(RCCE_FLAGS) 

RCCE_debug.o: $(RCCE_LIB_SRC)/RCCE_debug.c $(RCCEINCLUDE)/RCCE.h $(RCCEINCLUDE)/RCCE_lib.h \
         $(RCCEINCLUDE)/RCCE_debug.h
	$(CCOMPILE) -c $(CFLAGS) $(RCCE_LIB_SRC)/RCCE_debug.c  $(RCCE_FLAGS)

RCCE_comm.o: $(RCCE_LIB_SRC)/RCCE_comm.c $(RCCEINCLUDE)/RCCE.h $(RCCEINCLUDE)/RCCE_lib.h 
	$(CCOMPILE) -c $(CFLAGS) $(RCCE_LIB_SRC)/RCCE_comm.c  $(RCCE_FLAGS)

RCCE_send.o: $(RCCE_LIB_SRC)/RCCE_send.c $(RCCEINCLUDE)/RCCE.h $(RCCEINCLUDE)/RCCE_lib.h 
	$(CCOMPILE) -c $(CFLAGS) $(RCCE_LIB_SRC)/RCCE_send.c  $(RCCE_FLAGS)

RCCE_recv.o: $(RCCE_LIB_SRC)/RCCE_recv.c $(RCCEINCLUDE)/RCCE.h $(RCCEINCLUDE)/RCCE_lib.h 
	$(CCOMPILE) -c $(CFLAGS) $(RCCE_LIB_SRC)/RCCE_recv.c  $(RCCE_FLAGS)

RCCE_memcpy.o: $(RCCE_LIB_SRC)/RCCE_memcpy.c $(RCCEINCLUDE)/RCCE.h $(RCCEINCLUDE)/RCCE_lib.h 
	$(CCOMPILE) -c $(CFLAGS) $(RCCE_LIB_SRC)/RCCE_memcpy.c  $(RCCE_FLAGS)

RCCE_get.o: $(RCCE_LIB_SRC)/RCCE_get.c $(RCCEINCLUDE)/RCCE.h $(RCCEINCLUDE)/RCCE_lib.h 
	$(CCOMPILE) -c $(CFLAGS) $(RCCE_LIB_SRC)/RCCE_get.c  $(RCCE_FLAGS)

RCCE_put.o: $(RCCE_LIB_SRC)/RCCE_put.c $(RCCEINCLUDE)/RCCE.h $(RCCEINCLUDE)/RCCE_lib.h 
	$(CCOMPILE) -c $(CFLAGS) $(RCCE_LIB_SRC)/RCCE_put.c  $(RCCE_FLAGS)

RCCE_reduce.o: $(RCCE_LIB_SRC)/RCCE_reduce.c $(RCCEINCLUDE)/RCCE.h $(RCCEINCLUDE)/RCCE_lib.h 
	$(CCOMPILE) -c $(CFLAGS) $(RCCE_LIB_SRC)/RCCE_reduce.c  $(RCCE_FLAGS)

RCCE_bcast.o: $(RCCE_LIB_SRC)/RCCE_bcast.c $(RCCEINCLUDE)/RCCE.h $(RCCEINCLUDE)/RCCE_lib.h 
	$(CCOMPILE) -c $(CFLAGS) $(RCCE_LIB_SRC)/RCCE_bcast.c  $(RCCE_FLAGS)

RCCE_malloc.o: $(RCCE_LIB_SRC)/RCCE_malloc.c $(RCCEINCLUDE)/RCCE.h $(RCCEINCLUDE)/RCCE_lib.h 
	$(CCOMPILE) -c $(CFLAGS) $(RCCE_LIB_SRC)/RCCE_malloc.c  $(RCCE_FLAGS)

RCCE_shmalloc.o: $(RCCE_LIB_SRC)/RCCE_shmalloc.c $(RCCEINCLUDE)/RCCE.h $(RCCEINCLUDE)/RCCE_lib.h 
	$(CCOMPILE) -c $(CFLAGS) $(RCCE_LIB_SRC)/RCCE_shmalloc.c  $(RCCE_FLAGS)

RCCE_qsort.o: $(RCCE_LIB_SRC)/RCCE_qsort.c $(RCCEINCLUDE)/RCCE.h $(RCCEINCLUDE)/RCCE_lib.h 
	$(CCOMPILE) -c $(CFLAGS) $(RCCE_LIB_SRC)/RCCE_qsort.c  $(RCCE_FLAGS)

RCCE_synch.o: $(RCCE_LIB_SRC)/RCCE_synch.c $(RCCEINCLUDE)/RCCE.h $(RCCEINCLUDE)/RCCE_lib.h 
	$(CCOMPILE) -c $(CFLAGS) $(RCCE_LIB_SRC)/RCCE_synch.c  $(RCCE_FLAGS)

RCCE_flags.o: $(RCCE_LIB_SRC)/RCCE_flags.c $(RCCEINCLUDE)/RCCE.h $(RCCEINCLUDE)/RCCE_lib.h 
	$(CCOMPILE) -c $(CFLAGS) $(RCCE_LIB_SRC)/RCCE_flags.c  $(RCCE_FLAGS)

RCCE_emulator_driver.o: $(RCCE_LIB_SRC)/RCCE_emulator_driver.c $(RCCEINCLUDE)/RCCE.h 
	$(CCOMPILE) -c $(CFLAGS) $(RCCE_LIB_SRC)/RCCE_emulator_driver.c $(RCCE_FLAGS) 

SCC_API.o: $(RCCE_LIB_SRC)/SCC_API.c $(RCCEINCLUDE)/SCC_API.h 
	$(CCOMPILE) -c $(CFLAGS) $(RCCE_LIB_SRC)/SCC_API.c

RCCE_DCMflush.o: $(RCCE_LIB_SRC)/RCCE_DCMflush.c $(RCCEINCLUDE)/SCC_API.h 
	$(CCOMPILE) -c $(CFLAGS) $(RCCE_LIB_SRC)/RCCE_DCMflush.c

mpb.o: $(RCCE_LIB_SRC)/mpb.c $(RCCEINCLUDE)/SCC_API.h 
	$(CCOMPILE) -c $(CFLAGS) $(RCCE_LIB_SRC)/mpb.c

mpb: mpb.o SCC_API.o
	$(CCOMPILE) $(CFLAGS) mpb.o SCC_API.o -o $(RCCEROOT$)/bin/$(SUBDIR)/mpb
	rm -f *.o

clean:
	rm -f $(ARCHIVE) $(ARCHIVEOBJS)
	rm -f mpb.o $(RCCEROOT$)/bin/$(SUBDIR)/mpb
	rm -f bin/*/*.a

veryclean: 
	rm -f $(ARCHIVE) $(ARCHIVEOBJS)
	rm -f mpb.o $(RCCEROOT$)/bin/$(SUBDIR)/mpb
	rm -f bin/*/*.a
	cd apps/SHIFT;    make clean; cd -
	cd apps/STENCIL;  make clean; cd -
	cd apps/NPB;      make clean; cd -
	cd apps/PINGPONG; make clean; cd -
	cd apps/XHPL;     make veryclean; cd -
	cd apps/SHARE;    make clean; cd -
	rm -f common/symbols rccerun makeall
	@echo --------------------------------------------------------------------
	@echo RUN \"configure\" SCRIPT \(AGAIN\) BEFORE MAKING EXECUTABLES + LIBRARIES
	@echo --------------------------------------------------------------------        
