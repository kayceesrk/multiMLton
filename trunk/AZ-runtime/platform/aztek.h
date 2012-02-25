#define MLton_Platform_OS_host "aztek"

#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#include <sys/poll.h>
#include <sys/resource.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/fcntl.h>
#include <reent.h>
#include <sys/times.h>
#include <sys/utsname.h>
#include <sys/utime.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <os/mman.h>
#include <os/kernctl.h>
#include <os/sched.h>
#include <sys/signal.h>
//#include <tcpip/bsdsys/resource.h>
//#include <tcpip/bsdsys/socketvar.h>

#include <os/thread.h>
#include <os/mutex.h>
#include <azpr/azpr_atomic.h>


#define POINTER_BITS 64
#define ADDRESS_BITS 48

#define HAS_FEROUND FALSE
#define HAS_FPCLASSIFY FALSE
#define HAS_FPCLASSIFY32 FALSE
#define HAS_FPCLASSIFY64 FALSE
#define HAS_MSG_DONTWAIT TRUE
#define HAS_REMAP FALSE
#define HAS_SIGALTSTACK TRUE
#define HAS_SIGNBIT FALSE
#define HAS_SPAWN FALSE
#define HAS_TIME_PROFILING TRUE



#ifndef PRIu8 
#define PRIu8 "hhu" 
#endif 
#ifndef PRIu16 
#define PRIu16 "hu" 
#endif 
#ifndef PRIx16 
#define PRIx16 "hx" 
#endif 
#ifndef PRId32 
#define PRId32 "d" 
#endif 
#ifndef PRIu32 
#define PRIu32 "u" 
#endif
#ifndef PRIu64 
#define PRIu64 "llu"
#endif 

//Siddharth self defined
#ifndef PRIx8 
#define PRIx8 "x" 
#endif 

#ifndef PRIx64 
#define PRIx64 "x" 
#endif 

#ifndef PRIx32 
#define PRIx32 "x" 
#endif 
#ifndef INTMAX_MIN 
#define INTMAX_MIN LLONG_MIN 
#endif 
#ifndef PRIuMAX 
#define PRIuMAX "llu" 
#endif 
#ifndef PRIxMAX 
#define PRIxMAX "llx" 
#endif

#ifndef PRIxPTR
#define PRIxPTR "lx" 
#endif

#ifndef ccdefined
#define ccdefined
typedef unsigned char   MLton_cc_t;
typedef unsigned int    MLton_speed_t;
typedef unsigned int    MLton_tcflag_t;

#undef cc_t
#undef speed_t
#undef tcflag_t

#define cc_t MLton_cc_t
#define speed_t MLton_speed_t
#define tcflag_t MLton_tcflag_t
#endif

/* ------------------------------------------------- */
/*                   MLton.Rusage                    */
/* ------------------------------------------------- */

#ifndef RUSAGE_AZTEK
#define RUSAGE_AZTEK

#ifndef MLton_RUSAGE_STRUCT
#define MLton_RUSAGE_STRUCT
struct MLton_rusage {
        struct timeval ru_utime;
        struct timeval ru_stime;
};
#endif

//#undef rusage
//#define rusage MLton_rusage

//MLTON_WRAPPER
int MLton_getrusage (int who, struct rusage *usage);
#undef getrusage
#define getrusage MLton_getrusage
#endif

/* ------------------------------------------------- */
/*                   MLton.Rlimit                    */
/* ------------------------------------------------- */
#ifndef MLTON_RLIMIT_AZTEK
#define MLTON_RLIMIT_AZTEK


#ifndef RLIMIT_CPU
#define RLIMIT_CPU      0               /* CPU time in seconds */
#endif

#ifndef RLIMIT_FSIZE
#define RLIMIT_FSIZE    1               /* Maximum filesize */
#endif

#ifndef RLIMIT_DATA
#define RLIMIT_DATA     2               /* max data size */
#endif

#ifndef RLIMIT_STACK
#define RLIMIT_STACK    3               /* max stack size */
#endif

#ifndef RLIMIT_CORE
#define RLIMIT_CORE     4               /* max core file size */
#endif

#ifndef RLIMIT_NOFILE
#define RLIMIT_NOFILE   5               /* max number of open files */
#endif

#ifndef RLIMIT_OFILE
#define RLIMIT_OFILE    RLIMIT_NOFILE   /* BSD name */
#endif

#ifndef RLIMIT_AS
#define RLIMIT_AS       6               /* address space (virt. memory) limit */
#endif

#define RLIMIT_NLIMITS  7               /* upper bound of RLIMIT_* defines */
#define RLIM_NLIMITS    RLIMIT_NLIMITS

#ifndef RLIM_INFINITY
#define RLIM_INFINITY   (0xffffffffUL)
#endif

#ifndef RLIM_SAVED_MAX
#define RLIM_SAVED_MAX  RLIM_INFINITY
#endif

#ifndef RLIM_SAVED_CUR
#define RLIM_SAVED_CUR  RLIM_INFINITY
#endif

typedef unsigned long MLton_rlim_t;
#undef rlim_t
#define rlim_t MLton_rlim_t

struct MLton_rlimit {
        rlim_t  rlim_cur;
        rlim_t  rlim_max;
};
#undef rlimit
#define rlimit MLton_rlimit

//MLTON_WRAPPER 
int MLton_getrlimit (int resource, struct rlimit *rlim);
//MLTON_WRAPPER 
int MLton_setrlimit (int resource, const struct rlimit *rlim);
#undef getrlimit
#undef setrlimit
#define getrlimit MLton_getrlimit
#define setrlimit MLton_setrlimit

#endif

//#ifndef RLIM_T_AZTEK
//#define RLIM_T_AZTEK
//typedef unsigned int rlim_t;
//#endif



#ifndef NCCS
#define NCCS            18
#endif

#ifndef TERMIOS_AZTEK
#define TERMIOS_AZTEK
struct MLton_termios {
        cc_t c_cc[NCCS];
        tcflag_t c_cflag;
        tcflag_t c_iflag;
        tcflag_t c_lflag;
        tcflag_t c_oflag;
};
#undef termios
#define termios MLton_termios

#endif

