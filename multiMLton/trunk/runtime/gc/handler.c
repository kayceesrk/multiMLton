/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* GC_startSignalHandler does not do an enter()/leave(), even though
 * it is exported.  The basis library uses it via _import, not _prim,
 * and so does not treat it as a runtime call -- so the invariant in
 * enter would fail miserably.  It is OK because GC_startHandler must
 * be called from within a critical section.
 *
 * Don't make it inline, because it is also called in basis/Thread.c,
 * and when compiling with COMPILE_FAST, they may appear out of order.
 */
void GC_startSignalHandler (__attribute__ ((unused)) GC_state *gs) {
  /* Switch to the signal handler thread. */
  GC_state s = pthread_getspecific (gcstate_key);
  if (DEBUG_SIGNALS) {
    fprintf (stderr, "GC_startSignalHandler [%d]\n",
             Proc_processorNumber (s));
  }
  assert (s->atomicState == 1);
  assert (s->signalsInfo.signalIsPending);
  s->signalsInfo.signalIsPending = FALSE;
  s->signalsInfo.amInSignalHandler = TRUE;
  assert (s->savedThread == BOGUS_OBJPTR);
  s->savedThread = s->currentThread;
  /* Set s->atomicState to 2 when switching to the signal handler
   * thread; leaving the runtime will decrement s->atomicState to 1,
   * the signal handler will then run atomically and will finish by
   * switching to the thread to continue with, which will decrement
   * s->atomicState to 0.
   */
  s->atomicState = 2;
}

void GC_finishSignalHandler (__attribute__ ((unused)) GC_state *gs) {
  GC_state s = pthread_getspecific (gcstate_key);
  if (DEBUG_SIGNALS)
    fprintf (stderr, "GC_finishSignalHandler () [%d]\n",
             Proc_processorNumber (s));
  assert (s->atomicState == 1);
  s->signalsInfo.amInSignalHandler = FALSE;
}

void switchToSignalHandlerThreadIfNonAtomicAndSignalPending (GC_state s) {
   if (DEBUG_SIGNALS)
   {
    fprintf (stderr, "switchToSignalHandlerThreadIfNonAtomicAndSignalPending () [%d]\n",
             Proc_processorNumber (s));
    fprintf (stderr, "AtomicState %d\nsignalIsPending %d\n", s->atomicState, s->signalsInfo.signalIsPending);
   }
  if (s->atomicState == 1
      and s->signalsInfo.signalIsPending) {
    GC_startSignalHandler (&s->procStates);
    switchToThread (s, s->signalHandlerThread);
  }
}

/* GC_handler sets s->limit = 0 so that the next limit check will
 * fail.  Signals need to be blocked during the handler (i.e. it
 * should run atomically) because sigaddset does both a read and a
 * write of s->signalsInfo.signalsPending.  The signals are blocked
 * by Posix_Signal_handle (see Posix/Signal/Signal.c).
 */
void GC_handler (GC_state s, int signum) {
    int p = Proc_processorNumber (s);
  if (DEBUG_SIGNALS)
    fprintf (stderr, "GC_handler signum = %d [%d]\n", signum,
             p);
   //assert (sigismember (&s->signalsInfo.signalsHandled, signum));

  /* Cannot get the correct GC_state in the signal handler. Signals
   * are delivered to any of the threads which do not mask it. So
   * search for a GC_state which is registered to receive this signal
   * if such a GC_state is found, set signalIsPending flag and set
   * limit to 0 if atomicState is 0
   */
   for (int proc = 0; proc < s->numberOfProcs; proc++) {
      GC_state gcState = &s->procStates[proc];
      if (DEBUG_SIGNALS)
      {
        fprintf(stderr,"For processor %d\n",proc);
        fprintf(stderr,"sigismember? %d\n",sigismember(&gcState->signalsInfo.signalsHandled, signum));
        fprintf(stderr,"atomicState = %d\n",gcState->atomicState);
      }
      if(sigismember(&gcState->signalsInfo.signalsHandled, signum))
      {
        gcState->signalsInfo.signalIsPending = TRUE;
        sigaddset (&gcState->signalsInfo.signalsPending, signum);

        if (gcState->atomicState == 0)
        {
            gcState->limit = 0;
        }
        break;
      }
    }

  if (DEBUG_SIGNALS)
    fprintf (stderr, "GC_handler done [%d]\n",
             Proc_processorNumber (s));

}
