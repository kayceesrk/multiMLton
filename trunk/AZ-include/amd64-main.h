/* Copyright (C) 2000-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _AMD64_MAIN_H_
#define _AMD64_MAIN_H_

#include "common-main.h"

#ifndef DEBUG_AMD64CODEGEN
#define DEBUG_AMD64CODEGEN TRUE
#endif

#ifndef DEBUG_ALRM
#define DEBUG_ALRM FALSE
#endif

/* A key whose value will be a unique integer per thread */
extern C_Pthread_Key_t gcstate_key;

PRIVATE extern struct GC_state * gcState;

/* Globals */
PRIVATE Word64 applyFFTempFun;
PRIVATE Word64 applyFFTempStackArg;
PRIVATE Word64 applyFFTempRegArg[6];
PRIVATE Real32 applyFFTempXmmsRegArgD[8];
PRIVATE Real64 applyFFTempXmmsRegArgS[8];
PRIVATE Word32 checkTemp;
PRIVATE Word64 cReturnTemp[16];
PRIVATE Pointer c_stackP;
PRIVATE Word64 fpcvtTemp;
PRIVATE Word32 fpeqTemp;
PRIVATE Word64 divTemp;
PRIVATE Word64 indexTemp;
PRIVATE Word64 raTemp1;
PRIVATE Word64 spill[32];
PRIVATE Word64 stackTopTemp;

static GC_frameIndex returnAddressToFrameIndex (GC_returnAddress ra) {
        return *((GC_frameIndex*)(ra - sizeof(GC_frameIndex)));
}

#define MLtonCallFromC                                                  \
/* Globals */                                                           \
C_Pthread_Key_t gcstate_key;                                            \
PRIVATE void MLton_jumpToSML (pointer jump);                            \
static void MLton_callFromC (pointer ffiOpArgsResPtr) {                 \
        pointer jump;                                                   \
        GC_state s = pthread_getspecific (gcstate_key);                 \
                                                                        \
        if (DEBUG_AMD64CODEGEN)                                         \
                fprintf (stderr, "MLton_callFromC() starting\n");       \
        GC_setSavedThread (s, GC_getCurrentThread (s));                 \
        s->atomicState += 3;                                            \
        s->ffiOpArgsResPtr = ffiOpArgsResPtr;                           \
        if (s->signalsInfo.signalIsPending)                             \
                s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;       \
        /* Return to the C Handler thread. */                           \
        GC_switchToThread (s, GC_getCallFromCHandlerThread (s), 0);     \
        jump = *(pointer*)(s->stackTop - GC_RETURNADDRESS_SIZE);        \
        MLton_jumpToSML(jump);                                          \
        s->atomicState += 1;                                            \
        GC_switchToThread (s, GC_getSavedThread (s), 0);                \
        s->atomicState -= 1;                                            \
        if (0 == s->atomicState && s->signalsInfo.signalIsPending)      \
                s->limit = 0;                                           \
        if (DEBUG_AMD64CODEGEN)                                         \
                fprintf (stderr, "MLton_callFromC() done\n");           \
        return;                                                         \
}

#define MLtonMain(al, mg, mfs, mmc, pk, ps, ml)                         \
MLtonCallFromC                                                          \
void runAlrmHandler (void *arg) {                                       \
                                                                        \
        if (DEBUG_ALRM)                                                 \
            fprintf (stderr, "Running runAlrmHandler..\n");             \
        /* Save our state locally */                                    \
        GC_state s = (GC_state)arg;                                     \
        pthread_setspecific (gcstate_key, s);                           \
                                                                        \
        /* Block all signals */                                         \
        sigset_t blockSet;                                              \
        sigfillset (&blockSet);                                         \
        pthread_sigmask (SIG_SETMASK, &blockSet, NULL);                 \
                                                                        \
        if (DEBUG_ALRM)                                                 \
            fprintf (stderr, "Installing timer\n");                     \
        struct itimerval value, ovalue;                                 \
        int which = ITIMER_REAL;                                        \
        int microsec = (int)(s->timeInterval);                          \
        value.it_interval.tv_sec = microsec / 1000000;                  \
        value.it_interval.tv_usec = microsec % 1000000;                 \
        value.it_value.tv_sec = 0;                                      \
        value.it_value.tv_usec = microsec*5;                            \
                                                                        \
        while (not Proc_isInitialized (s)) {}                           \
         setitimer( which, &value, &ovalue );                           \
                                                                        \
        sigset_t set;                                                   \
        /* XXX To delay sending SigUsr2 by one one time quantum. CML signal */      \
        /* handler might not be installed if signal is sent immediately */    \
        bool sendSigUsr2 = FALSE;                                       \
        sigemptyset (&set);                                             \
        sigaddset (&set, SIGALRM);                                      \
                                                                        \
        GC_state gcState0 = &s->procStates[0];                          \
        sigaddset(&gcState0->signalsInfo.signalsHandled, SIGALRM);      \
                                                                        \
        while (1) {                                                     \
            int signum;                                                 \
            if (DEBUG_ALRM)                                             \
                fprintf (stderr, "Wait for alrm\n");                    \
            sigwait (&set, &signum);                                    \
            /* set up switches if GC_state is registered for an alrm */ \
            for (int proc = 0; proc < s->numberOfProcs; proc++) {       \
                GC_state gcState = &s->procStates[proc];                \
                if (DEBUG_ALRM)                                         \
                {                                                       \
                    fprintf(stderr,"Got an ALRM\n");                    \
                    fprintf(stderr,"For processor %d\n",proc);          \
                    fprintf(stderr,"sigismember? SIGALRM %d\n",sigismember(&gcState->signalsInfo.signalsHandled, SIGALRM)); \
                    fprintf(stderr,"sigismember? SIGUSR2 %d\n",sigismember(&gcState->signalsInfo.signalsHandled, SIGUSR2)); \
                    fprintf(stderr,"inGC? %d\n", gcState->amInGC);      \
                    fprintf(stderr,"inHandler? %d\n", gcState->signalsInfo.amInSignalHandler); \
                    fprintf(stderr,"atomicState = %d\n",gcState->atomicState); \
                    fprintf(stderr,"sendSigUsr2 = %d\n", sendSigUsr2);  \
                }                                                       \
                /*Parallel_wakeUpThread (proc);*/                       \
                if(sigismember(&gcState->signalsInfo.signalsHandled, SIGALRM)) \
                {                                                       \
                    if (!gcState->signalsInfo.amInSignalHandler && !gcState->signalsInfo.signalIsPending) { \
                        if (gcState->atomicState == 0)                  \
                            gcState->limit = 0;                         \
                        gcState->signalsInfo.signalIsPending = TRUE;    \
                        sigaddset (&gcState->signalsInfo.signalsPending, SIGALRM); \
                    }                                                   \
                }                                                       \
                /* if SIGUSR2 is handled */                             \
                if (sigismember(&gcState->signalsInfo.signalsHandled, SIGUSR2) &&   \
                    !gcState->signalsInfo.amInSignalHandler &&          \
                    !gcState->amInGC) {                                 \
                    if (sendSigUsr2) {                                  \
                        pthread_kill (gcState->pthread, SIGUSR2);       \
                    }                                                   \
                    sendSigUsr2 = TRUE;                                 \
                }                                                       \
            }                                                           \
        }                                                               \
}                                                                       \
                                                                        \
void run (void *arg) {                                                  \
        pointer jump;                                                   \
        extern pointer ml;                                              \
        GC_state s = (GC_state)arg;                                     \
        if (s == NULL) {                                                \
            fprintf (stderr, "gcState is NULL\n");                      \
            exit (1);                                                   \
        }                                                               \
        uint32_t num = Proc_processorNumber (s)                         \
                * s->controls->affinityStride                           \
                + s->controls->affinityBase;                            \
         set_cpu_affinity(num);                                         \
                                                                        \
        /* Save our state locally */                                    \
        pthread_setspecific (gcstate_key, s);                           \
        s->pthread = pthread_self ();                                   \
        /* Mask ALRM signal */                                          \
        if (s->enableTimer)                                             \
        {                                                               \
        sigset_t set;                                                   \
        sigemptyset (&set);                                             \
        sigaddset (&set, SIGALRM);                                      \
        pthread_sigmask (SIG_SETMASK, &set, NULL);                      \
        }                                                               \
                                                                        \
        if (s->amOriginal) {                                            \
                fprintf (stderr, "Real init\n");                        \
                real_Init();                                            \
                jump = (pointer)&ml;                                    \
        } else {                                                        \
                /* Return to the saved world */                         \
                jump = *(pointer*)(s->stackTop - GC_RETURNADDRESS_SIZE); \
        }                                                               \
        /* Check to see whether or not we are the first thread */       \
        if (Proc_amPrimary (s)) {                                       \
            fprintf (stderr, "main jump\n");                             \
            MLton_jumpToSML(jump);                                      \
        }                                                               \
        else {                                                          \
                Proc_waitForInitialization (s);                         \
                Parallel_run ();                                        \
        }                                                               \
}                                                                       \
                                                                        \
PUBLIC int MLton_main (int argc, char* argv[]) {                        \
        int procNo;                                                     \
        pthread_t *threads;                                             \
        {                                                               \
                struct GC_state s;                                      \
                /* Initialize with a generic state to read in @MLtons, etc */ \
                Initialize (s, al, mg, mfs, mmc, pk, ps, 0);            \
                                                                        \
                threads = (pthread_t *) malloc ((s.numberOfProcs) * sizeof (pthread_t)); \
                gcState = (GC_state) malloc ((s.numberOfProcs+1) * sizeof (struct GC_state)); \
                /* Create key */                                        \
                if (pthread_key_create(&gcstate_key, NULL)) {           \
                        fprintf (stderr, "pthread_key_create failed: %s\n", strerror (errno)); \
                        exit (1);                                       \
                }                                                       \
                /* Now copy initialization to the first processor state */      \
                memcpy (&gcState[0], &s, sizeof (struct GC_state));     \
                gcState[0].procStates = gcState;                        \
                GC_lateInit (&gcState[0]);                              \
        }                                                               \
        /* Fill in per-processor data structures */                     \
        for (procNo = 1; procNo <= gcState[0].numberOfProcs; procNo++) { \
                Duplicate (&gcState[procNo], &gcState[0]);              \
                gcState[procNo].procStates = gcState;                   \
                if (procNo == gcState[0].numberOfProcs)                 \
                    gcState[procNo].atomicState = 0;                    \
        }                                                               \
        /* Now create the threads */                                    \
        for (procNo = 1; procNo < gcState[0].numberOfProcs; procNo++) { \
                if (pthread_create (&threads[procNo - 1], NULL, &run, (void *)&gcState[procNo])) { \
                        fprintf (stderr, "pthread_create failed: %s\n", strerror (errno)); \
                        exit (1);                                       \
                }                                                       \
        }                                                               \
        /* Create the alrmHandler thread */                             \
        if(gcState[0].enableTimer) {                                    \
        if(pthread_create (&threads[gcState[0].numberOfProcs -1], NULL, &runAlrmHandler, (void*)&gcState[gcState[0].numberOfProcs])) { \
                fprintf (stderr, "pthread_create failed: %s\n", strerror (errno)); \
                exit (1);                                               \
        }                                                               \
        }                                                               \
        run ((void *)&gcState[0]);                                      \
}

#define MLtonLibrary(al, mg, mfs, mmc, pk, ps, ml)                      \
MLtonCallFromC                                                          \
PUBLIC void LIB_OPEN(LIBNAME) (int argc, char* argv[]) {                \
        pointer jump;                                                   \
        extern pointer ml;                                              \
                                                                        \
        gcState = (GC_state) malloc (sizeof (struct GC_state));         \
        Initialize (gcState[0], al, mg, mfs, mmc, pk, ps, 0);           \
        GC_state s = &gcState[0];                                       \
        if (s->amOriginal) {                                            \
                real_Init();                                            \
                jump = (pointer)&ml;                                    \
        } else {                                                        \
                jump = *(pointer*)(s->stackTop - GC_RETURNADDRESS_SIZE); \
        }                                                               \
        MLton_jumpToSML(jump);                                          \
}                                                                       \
PUBLIC void LIB_CLOSE(LIBNAME) () {                                     \
        pointer jump;                                                   \
        GC_state s = &gcState[0];                                       \
        jump = *(pointer*)(s->stackTop - GC_RETURNADDRESS_SIZE);   \
        MLton_jumpToSML(jump);                                          \
}

#endif /* #ifndef _AMD64_MAIN_H_ */
