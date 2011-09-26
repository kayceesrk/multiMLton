SHELL=/bin/sh
BENCHMARK=lu
BENCHMARKU=LU

PROGRAM  = $(BENCHMARK).$(CLASS).$(NPROCS)

default:: ${PROGRAM}

# This makes sure the configuration utility setparams 
# is up to date. 
# Note that this must be run every time, which is why the
# target does not exist and is not created. 
# If you create a file called "config" you will break things. 
config:
	cd ../sys; make all
	../sys/setparams ${BENCHMARK} ${NPROCS} ${CLASS} ${SUBTYPE}

# Normally setparams updates npbparams.h only if the settings (CLASS/NPROCS)
# have changed. However, we also want to update if the compile options
# may have changed (set in ../config/make.def). 
npbparams.h: ../config/make.def
	@ echo make.def modified. Rebuilding npbparams.h just in case
	rm -f npbparams.h
	../sys/setparams ${BENCHMARK} ${NPROCS} ${CLASS}

# So that "make benchmark-name" works
${BENCHMARK}:  default
${BENCHMARKU}: default


lu.o:		lu.c applu.h npbparams.h applu_protos.h
blts.o:	        blts.c applu_share.h applu_protos.h npbparams.h
buts.o:	        buts.c applu_share.h applu_protos.h npbparams.h
erhs.o:		erhs.c npbparams.h applu_share.h applu_protos.h 
error.o:	error.c npbparams.h applu_share.h applu_protos.h 
exact.o:	exact.c npbparams.h applu_share.h applu_protos.h
exchange_1.o:	exchange_1.c npbparams.h  applu_share.h applu_protos.h
exchange_3.o:	exchange_3.c npbparams.h  applu_share.h applu_protos.h
exchange_4.o:	exchange_4.c npbparams.h  applu_share.h applu_protos.h
exchange_5.o:	exchange_5.c npbparams.h  applu_share.h applu_protos.h
exchange_6.o:	exchange_6.c npbparams.h  applu_share.h applu_protos.h
init_comm.o:	init_comm.c npbparams.h   applu_share.h applu_protos.h
jacld.o:	jacld.c npbparams.h applu_share.h applu_protos.h
jacu.o:		jacu.c npbparams.h applu_share.h applu_protos.h
l2norm.o:	l2norm.c  applu_share.h applu_protos.h
neighbors.o:	neighbors.c npbparams.h applu_share.h applu_protos.h
nodedim.o:	nodedim.c applu_share.h applu_protos.h
pintgr.o:	pintgr.c npbparams.h  applu_share.h applu_protos.h
proc_grid.o:	proc_grid.c npbparams.h applu_share.h applu_protos.h
read_input.o:	read_input.c npbparams.h  applu_share.h applu_protos.h
rhs.o:		rhs.c npbparams.h applu_share.h applu_protos.h
setbv.o:	setbv.c npbparams.h applu_share.h applu_protos.h
setiv.o:	setiv.c npbparams.h applu_share.h applu_protos.h
setcoeff.o:	setcoeff.c npbparams.h applu_share.h applu_protos.h
ssor.o:		ssor.c npbparams.h  applu_share.h applu_protos.h
subdomain.o:	subdomain.c npbparams.h  applu_share.h applu_protos.h
verify.o:	verify.c npbparams.h applu_share.h applu_protos.h

OBJS = lu.o init_comm.o read_input.o proc_grid.o neighbors.o \
       nodedim.o subdomain.o setcoeff.o  setbv.o exact.o setiv.o \
       erhs.o ssor.o exchange_1.o exchange_3.o \
       exchange_4.o exchange_5.o exchange_6.o \
       pintgr.o l2norm.o error.o \
       rhs.o jacld.o blts.o jacu.o buts.o \
       verify.o print_results.o timers.o $(ARCHIVE)

${PROGRAM}: $(OBJS)
	${CCOMPILE} ${CFLAGS} -o ${PROGRAM} ${OBJS}
# use line below for gcc, which does not link libm by default
#	${CCOMPILE} ${CFLAGS} -o ${PROGRAM} ${OBJS} -lm

.c.o:
	${CCOMPILE} -c $(CFLAGS) -restrict $<

clean:
	- /bin/rm -f npbparams.h
	- /bin/rm -f *.o *~
