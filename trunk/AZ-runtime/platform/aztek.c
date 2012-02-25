#include "platform.h"

#include <ieeefp.h>
#include "diskBack.unix.c"
#include "displayMem.proc.c"
//#include "mmap-protect.c"
//#include "nonwin.c"
#include "sysconf.c"
#include "use-mmap.c"
#include "softfloat/softfloat.h"

/*
 static void catcher (__attribute__ ((unused)) int signo,
                     __attribute__ ((unused)) siginfo_t* info,
                     void* context) {
        ucontext_t* ucp = (ucontext_t*)context;
        GC_handleSigProf ((code_pointer) ucp->sc_eip);
}
*/
//#define SA_ONSTACK //???? Siddharth need?
void GC_setSigProfHandler (struct sigaction *sa) {
        //sa->sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
        //sa->sa_sigaction = (void (*)(int, siginfo_t*, void*))catcher;
}


/* ------------------------------------------------- */
/*                   MLton.Rlimit                    */
/* ------------------------------------------------- */

static struct rlimit rlimits[RLIM_NLIMITS];

static void initRlimits (void) {
        static int done = FALSE;
        int lim;

	printf("AZTEK: initRlimits\n");

        if (done)
                return;
        done = TRUE;
        for (lim = 0; lim < RLIM_NLIMITS; ++lim ) {
                rlimits[lim].rlim_cur = 0;
                rlimits[lim].rlim_max = UINT_MAX;
        }
}

int getrlimit (int resource, struct rlimit *rlp) {
	printf("AZTEK: getrlimit\n");
        initRlimits ();
        if (resource < 0 or resource >= RLIM_NLIMITS) {
                errno = EINVAL;
                return -1;
        }
        *rlp = rlimits[resource];
        return 0;
}

int setrlimit (int resource, const struct rlimit *rlp) {
	printf("AZTEK: setrlimit\n");
        initRlimits ();
        if (resource < 0 or resource >= RLIM_NLIMITS) {
                errno = EINVAL;
                return -1;
        }
        if (rlp->rlim_cur < rlimits[resource].rlim_max)
                rlimits[resource].rlim_cur = rlp->rlim_cur;
        else {
                errno = EPERM;
                return -1;
        }
        rlimits[resource].rlim_max = rlp->rlim_max;
        return 0;
}

/* ------------------------------------------------- */
/*                   MLton.Rusage                    */
/* ------------------------------------------------- */

//MLTON_WRAPPER
int MLton_getrusage (int who, struct rusage *usage){
  //printf("AZTEK: MLton_getrusage\n");
  return 0;
}

fp_rnd fpsetround(fp_rnd rnd_dir)
{
  fp_rnd old;

  old = float_rounding_mode;
  float_rounding_mode = rnd_dir;
  return old;
}

fp_rnd
fpgetround(void)
{
  return float_rounding_mode;
}

int fegetround (void) {
        int mode;

        mode = fpgetround ();
        switch (mode) {
        case FP_RN: mode = 0; break;
        case FP_RM: mode = 1; break;
        case FP_RP: mode = 2; break;
        case FP_RZ: mode = 3; break;
        default:
                die ("fegetround: invalid mode %d\n", mode);
        }
        return mode;
}

int fesetround (int mode) {
        switch (mode) {
        case 0: mode = FP_RN; break;
        case 1: mode = FP_RM; break;
        case 2: mode = FP_RP; break;
        case 3: mode = FP_RZ; break;
        default:
                die ("fesetround: invalid mode %d\n", mode);
        }
        fpsetround (mode);
        return 0;
}

