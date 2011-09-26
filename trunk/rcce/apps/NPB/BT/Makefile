SHELL=/bin/sh
BENCHMARK=bt
BENCHMARKU=BT

PROGRAM  = $(BENCHMARK).$(CLASS).$(NPROCS)

default:: ${PROGRAM}

# This makes sure the configuration utility setparams 
# is up to date. 
# Note that this must be run every time, which is why the
# target does not exist and is not created. 
# If you create a file called "config" you will break things. 
config:
	cd ../sys; ${MAKE} all
	../sys/setparams ${BENCHMARK} ${NPROCS} ${CLASS}

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

bt.o:             bt.c  header.h npbparams.h  mpinpb.h
make_set.o:       make_set.c  header.h npbparams.h  mpinpb.h
initialize.o:     initialize.c  header.h npbparams.h
exact_solution.o: exact_solution.c  header.h npbparams.h
exact_rhs.o:      exact_rhs.c  header.h npbparams.h
set_constants.o:  set_constants.c  header.h npbparams.h
adi.o:            adi.c  header.h npbparams.h
define.o:         define.c  header.h npbparams.h
copy_faces.o:     copy_faces.c  header.h npbparams.h  mpinpb.h
rhs.o:            rhs.c  header.h npbparams.h
x_solve.o:        x_solve.c  header.h work_lhs.h npbparams.h  mpinpb.h
y_solve.o:        y_solve.c  header.h work_lhs.h npbparams.h  mpinpb.h
z_solve.o:        z_solve.c  header.h work_lhs.h npbparams.h  mpinpb.h
solve_subs.o:     solve_subs.c  npbparams.h
add.o:            add.c  header.h npbparams.h
error.o:          error.c  header.h npbparams.h  mpinpb.h
verify.o:         verify.c  header.h npbparams.h  mpinpb.h
setup_mpi.o:      setup_mpi.c mpinpb.h npbparams.h 


OBJS = bt.o make_set.o initialize.o exact_solution.o \
       exact_rhs.o set_constants.o adi.o define.o copy_faces.o  \
       rhs.o x_solve.o y_solve.o z_solve.o add.o solve_subs.o   \
       error.o verify.o setup_mpi.o print_results.o timers.o $(ARCHIVE) 

$(PROGRAM): ${OBJS} 
	${CCOMPILE} ${CFLAGS} -o ${PROGRAM} ${OBJS} 
# use line below for gcc, which does not link libm by default
#	${CCOMPILE} ${CFLAGS} -o ${PROGRAM} ${OBJS} -lm

.c.o:
	${CCOMPILE} -c $(CFLAGS)  $<

clean:
	- rm -f *.o *~ mputil*
	- rm -f  npbparams.h core
