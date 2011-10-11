/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _C_MAIN_H_
#define _C_MAIN_H_

#include "common-main.h"
#include "c-common.h"
#include <execinfo.h>

static GC_frameIndex returnAddressToFrameIndex (GC_returnAddress ra) {
        return (GC_frameIndex)ra;
}

#define MLtonCallFromC                                                  \
/* Globals */                                                           \
C_Pthread_Key_t gcstate_key;                                            \
void MLton_callFromC (pointer ffiOpArgsResPtr) {                        \
        struct cont cont;                                               \
        GC_state s = pthread_getspecific (gcstate_key);                 \
                                                                        \
        if (DEBUG_CCODEGEN)                                             \
                fprintf (stderr, "MLton_callFromC() starting\n");       \
        GC_setSavedThread (s, GC_getCurrentThread (s));                 \
        s->atomicState += 3;                                            \
        s->ffiOpArgsResPtr = ffiOpArgsResPtr;                           \
        if (s->signalsInfo.signalIsPending)                             \
                s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;       \
        /* Switch to the C Handler thread. */                           \
        GC_switchToThread (s, GC_getCallFromCHandlerThread (s), 0);     \
        cont.nextFun = *(uintptr_t*)(s->stackTop - GC_RETURNADDRESS_SIZE); \
        cont.nextChunk = nextChunks[cont.nextFun];                      \
        s->returnToC = FALSE;                                           \
        do {                                                            \
                cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun); \
        } while (not s->returnToC);                                     \
        s->returnToC = FALSE;                                           \
        printf ("\nMLtonCallFromC");                                    \
        s->atomicState += 1;                                            \
        GC_switchToThread (s, GC_getSavedThread (s), 0);                \
        s->atomicState -= 1;                                            \
        if (0 == s->atomicState                                         \
            && s->signalsInfo.signalIsPending)                          \
                s->limit = 0;                                           \
        if (DEBUG_CCODEGEN)                                             \
                fprintf (stderr, "MLton_callFromC done\n");             \
}

#define MLtonMain(al, mg, mfs, mmc, pk, ps, gnr, mc, ml)                \
MLtonCallFromC                                                          \
                                                                        \
void runProfHandler (void* arg) {                                       \
        if (DEBUG_ALRM)                                                 \
            fprintf (stderr, "Running runProfHandler..\n");             \
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
            fprintf (stderr, "Installing itimer_prof\n");               \
                                                                        \
        /* Set PROF timer */                                            \
        struct itimerval value;                                         \
        value.it_interval.tv_sec = 0;                                   \
        value.it_interval.tv_usec = 10000;                              \
        value.it_value.tv_sec = 0;                                      \
        value.it_value.tv_usec = 10000;                                 \
        while (not Proc_isInitialized (s)) {}                           \
        setitimer(ITIMER_PROF, &value, NULL);                           \
                                                                        \
        sigset_t set;                                                   \
        sigemptyset (&set);                                             \
        sigaddset (&set, SIGPROF);                                      \
                                                                        \
        while (1) {                                                     \
            int signum;                                                 \
            if (DEBUG_ALRM)                                             \
                fprintf (stderr, "Wait for prof timer\n");              \
            sigwait (&set, &signum);                                    \
            for (int proc = 0; proc < s->numberOfProcs; proc++) {       \
               GC_state gcState = &s->procStates[proc];                 \
               if (gcState->profiling.isProfilingTimeOn)                \
                 pthread_kill (gcState->pthread, SIGUSR1);              \
           }                                                            \
        }                                                               \
}                                                                       \
                                                                        \
void runAlrmHandler (void *arg) {                                       \
        if (DEBUG_ALRM)                                                 \
            fprintf (stderr, "Running runAlrmHandler..\n");             \
                                                                        \
        /* XXX KC is this needed?? */                                   \
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
                                                                        \
        /* Set REAL timer */                                            \
        struct itimerval value;                                         \
        int microsec = (int)(s->timeInterval);                          \
        value.it_interval.tv_sec = microsec / 1000000;                  \
        value.it_interval.tv_usec = microsec % 1000000;                 \
        value.it_value.tv_sec = 0;                                      \
        value.it_value.tv_usec = microsec*5;                            \
                                                                        \
        while (not Proc_isInitialized (s)) {}                           \
        setitimer(ITIMER_REAL, &value, NULL);                           \
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
                fprintf (stderr, "Wait for alrm \n");                   \
            sigwait (&set, &signum);                                    \
                                                                        \
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
                                                                        \
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
void segvHandler (int sig) {                                            \
  void* tracePtrs[100];                                                 \
  int count = backtrace( tracePtrs, 100 );                              \
  char** funcNames = backtrace_symbols( tracePtrs, count );             \
  /* Print the stack trace */                                           \
  for( int ii = 0; ii < count; ii++ )                                   \
        printf( "%s\n", funcNames[ii] );                                \
  /* Free the string pointers */                                        \
  free( funcNames );                                                    \
  exit (0);                                                             \
}                                                                       \
                                                                        \
void run (void *arg) {                                                  \
        struct cont cont;                                               \
        signal (SIGSEGV, segvHandler);                                  \
        signal (SIGABRT, segvHandler);                                  \
        GC_state s = (GC_state)arg;                                     \
        uint32_t num = Proc_processorNumber (s)                         \
                * s->controls->affinityStride                           \
                + s->controls->affinityBase;                            \
         int numCompute = s->numberOfProcs - s->numIOThreads;           \
         set_cpu_affinity(num);                                         \
                                                                        \
        /* Save our state locally */                                    \
        pthread_setspecific (gcstate_key, s);                           \
        s->pthread = pthread_self ();                                   \
                                                                        \
        /* Mask ALRM and PROF signal */                                 \
        sigset_t blockSet;                                              \
        sigemptyset (&blockSet);                                        \
        if (s->enableTimer)                                             \
            sigaddset (&blockSet, SIGALRM);                             \
        if (s->profiling.kind == PROFILE_TIME_FIELD ||                  \
            s->profiling.kind == PROFILE_TIME_LABEL)                    \
            sigaddset (&blockSet, SIGPROF);                             \
        pthread_sigmask (SIG_SETMASK, &blockSet, NULL);                 \
                                                                        \
        if (s->amOriginal) {                                            \
                real_Init();                                            \
                PrepFarJump(cont, mc, ml);                              \
        } else {                                                        \
                /* Return to the saved world */                         \
                cont.nextFun = *(uintptr_t*)(s->stackTop - GC_RETURNADDRESS_SIZE); \
                cont.nextChunk = nextChunks[cont.nextFun];              \
        }                                                               \
        /* Check to see whether or not we are the first thread */       \
        if (Proc_amPrimary (s)) {                                       \
                /* Trampoline */                                        \
                while (1) {                                             \
                        cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun); \
                        cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun); \
                        cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun); \
                        cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun); \
                        cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun); \
                        cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun); \
                        cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun); \
                        cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun); \
                }                                                       \
        }                                                               \
        else {                                                          \
                Proc_waitForInitialization (s);                         \
                Parallel_run ();                                        \
        }                                                               \
}                                                                       \
PUBLIC int MLton_main (int argc, char* argv[]) {                        \
        int procNo;                                                     \
        pthread_t *threads;                                             \
        pthread_t alrmHandlerThread;                                    \
        pthread_t profHandlerThread;                                    \
        {                                                               \
                struct GC_state s;                                      \
                /* Initialize with a generic state to read in @MLtons, etc */ \
                Initialize (s, al, mg, mfs, mmc, pk, ps, gnr);          \
                                                                        \
                threads = (pthread_t *) malloc ((s.numberOfProcs-1) * sizeof (pthread_t)); \
                gcState = (GC_state) malloc ((s.numberOfProcs+1) * sizeof (struct GC_state)); \
                /* Create key */                                        \
                if (pthread_key_create(&gcstate_key, NULL)) {           \
                        fprintf (stderr, "pthread_key_create failed: %s\n", strerror (errno)); \
                        exit (1);                                       \
                }                                                       \
                /* Now copy initialization to the first processor state */      \
                memcpy (&gcState[0], &s, sizeof (struct GC_state));     \
                gcState[0].procStates = gcState;                        \
                gcState[0].procId = 0;                                  \
                GC_lateInit (&gcState[0]);                              \
        }                                                               \
        /* Fill in per-processor data structures */                     \
        for (procNo = 1; procNo <= gcState[0].numberOfProcs; procNo++) { \
                Duplicate (&gcState[procNo], &gcState[0]);              \
                gcState[procNo].procStates = gcState;                   \
                gcState[procNo].procId = procNo;                        \
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
            if(pthread_create (&alrmHandlerThread, NULL, &runAlrmHandler, (void*)&gcState[gcState[0].numberOfProcs])) { \
                    fprintf (stderr, "pthread_create failed: %s\n", strerror (errno)); \
                    exit (1);                                           \
            }                                                           \
        }                                                               \
        if(gcState[0].profiling.kind == PROFILE_TIME_LABEL ||           \
           gcState[0].profiling.kind == PROFILE_TIME_FIELD) {           \
             if(pthread_create (&profHandlerThread, NULL, &runProfHandler, (void*)&gcState[0])) { \
                    fprintf (stderr, "pthread_create failed: %s\n", strerror (errno)); \
                    exit (1);                                           \
            }                                                           \
        }                                                               \
        run ((void *)&gcState[0]);                                      \
}

/*XXX KC : Not sure if gcState is correct*/
/* Not even sure what this does */
#define MLtonLibrary(al, mg, mfs, mmc, pk, ps, gnr, mc, ml)             \
MLtonCallFromC                                                          \
PUBLIC void LIB_OPEN(LIBNAME) (int argc, char* argv[]) {                \
        struct cont cont;                                               \
        gcState = (GC_state) malloc (sizeof (struct GC_state));         \
        Initialize (gcState[0], al, mg, mfs, mmc, pk, ps, gnr);         \
        GC_state s = &gcState[0];                                       \
        if (s->amOriginal) {                                            \
                real_Init();                                            \
                PrepFarJump(cont, mc, ml);                              \
        } else {                                                        \
                /* Return to the saved world */                         \
                cont.nextFun = *(uintptr_t*)(s->stackTop - GC_RETURNADDRESS_SIZE); \
                cont.nextChunk = nextChunks[cont.nextFun];              \
        }                                                               \
        /* Trampoline */                                                \
        s->returnToC = FALSE;                                           \
        do {                                                            \
                cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun); \
        } while (not s->returnToC);                                     \
}                                                                       \
PUBLIC void LIB_CLOSE(LIBNAME) () {                                     \
        struct cont cont;                                               \
        GC_state s = &gcState[0];                                       \
        cont.nextFun = *(uintptr_t*)(s->stackTop - GC_RETURNADDRESS_SIZE); \
        cont.nextChunk = nextChunks[cont.nextFun];                      \
        s->returnToC = FALSE;                                           \
        do {                                                            \
                cont=(*(struct cont(*)(uintptr_t))cont.nextChunk)(cont.nextFun); \
        } while (not s->returnToC);                                     \
        GC_done(s);                                                     \
}

#endif /* #ifndef _C_MAIN_H */
