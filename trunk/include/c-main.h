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
#include "RCCE.h"
#include <unistd.h>

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
        pthread_setspecific (gcstate_key, s);                           \
                                                                        \
        /* Save our state locally */                                    \
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
        if (Proc_amPrimary (s)) {                                       \
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
                Parallel_assistInit (s);                                \
                Proc_waitForInitialization (s);                         \
                Parallel_run ();                                        \
        }                                                               \
}                                                                       \
PUBLIC int MLton_main (int argc, char* argv[]) {                        \
        dup2(STDOUT_FILENO, STDERR_FILENO);                             \
        RCCE_init (&argc, &argv);                                       \
        RCCE_barrier (&RCCE_COMM_WORLD);                                \
        pthread_t *threads;                                             \
        pthread_t alrmHandlerThread;                                    \
        pthread_t profHandlerThread;                                    \
        if (pthread_key_create (&gcstate_key, NULL)) {                  \
          fprintf (stderr, "pthread_key_create failed\n");              \
          exit (1);                                                     \
        }                                                               \
        struct GC_state s;                                              \
        s.procId = RCCE_ue ();                                          \
        createGlobals ();                                               \
        if (RCCE_ue () == 0) {                                          \
          /* Initialize with a generic state to read in @MLtons, etc */ \
          Initialize (s, al, mg, mfs, mmc, pk, ps, gnr);                \
          GC_lateInit (&s);                                             \
          /* Bcast s0 for duplication */                                \
          RCCE_bcast ((char*)&s, sizeof (struct GC_state), 0, RCCE_COMM_WORLD); \
          setSharedHeapState (&s, TRUE);                                \
        }                                                               \
        if (RCCE_ue () != 0) {                                          \
          struct GC_state s0;                                           \
          /* Recv s0 and duplicate */                                   \
          RCCE_bcast ((char*)&s0, sizeof (struct GC_state), 0, RCCE_COMM_WORLD); \
          Duplicate (&s, &s0);                                          \
          assistSetSharedHeapState (&s, TRUE);                          \
        }                                                               \
        RCCE_barrier (&RCCE_COMM_WORLD);                                \
        run ((void *)&s);                                               \
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
