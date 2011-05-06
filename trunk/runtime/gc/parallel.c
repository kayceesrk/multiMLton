
#include <pthread.h>
#include <time.h>
#include "platform.h"

/* num of holding thread or -1 if no one*/
volatile int32_t *Parallel_mutexes;
pthread_mutex_t *waitMutex;
pthread_cond_t *waitCondVar;
bool* dataInMutatorQ;

void Parallel_initResources (GC_state s) {
  Parallel_mutexes = (int32_t *) malloc (s->numberOfProcs * sizeof (int32_t));
  waitMutex = (pthread_mutex_t*) malloc (s->numberOfProcs * sizeof (pthread_mutex_t));
  waitCondVar = (pthread_cond_t*) malloc (s->numberOfProcs * sizeof (pthread_cond_t));
  dataInMutatorQ = (bool*) malloc (s->numberOfProcs * sizeof(bool));
  for (int proc = 0; proc < s->numberOfProcs; proc++) {
    Parallel_mutexes[proc] = -1;
    pthread_mutex_init (&waitMutex[proc], NULL);
    pthread_cond_init (&waitCondVar[proc], NULL);
    /* To be on the safe side initialize dataInMutatorQ with true. This will be cleared
     * on the first iteration if it is a false positive */
    dataInMutatorQ[proc] = TRUE;
  }
}

void Parallel_init (void) {
  GC_state s = pthread_getspecific (gcstate_key);

  if (!Proc_isInitialized (s)) {
    /* Move the object pointers in call-from-c-handler stack to the shared heap in
     * preparation for copying this stack to each processor */
    {
      GC_thread thrd = (GC_thread) objptrToPointer (s->callFromCHandlerThread, s->heap->start);
      pointer stk = objptrToPointer (thrd->stack, s->heap->start);
      moveEachObjptrInObject (s, stk);
    }

    /* Set up call-back state in each worker thread */
    /* XXX hack copy the call-from-c-handler into the worker threads
       assumes this is called by the primary thread */
    for (int proc = 0; proc < s->numberOfProcs; proc++)
      s->procStates[proc].callFromCHandlerThread =
        pointerToObjptr (copyThreadTo (s, &s->procStates[proc],
                                       objptrToPointer(s->callFromCHandlerThread, s->heap->start)), s->heap->start);

    /* Lift all objects from local heap of processor 0 to the shared heap. This
     * must come before waking up the processors */
    if (s->numberOfProcs != 1)
      liftAllObjectsDuringInit (s);

    /* Now wake them up! */
    Proc_signalInitialization (s);
  }
}

Int32 Parallel_processorNumber (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  return Proc_processorNumber (s);
}

Int32 Parallel_numberOfProcessors (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  return s->numberOfProcs;
}

Int32 Parallel_numIOThreads (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  return s->numIOThreads;
}

Word64 Parallel_maxBytesLive (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  return (uint64_t)s->cumulativeStatistics->maxBytesLiveSinceReset;
}

void Parallel_resetBytesLive (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  s->cumulativeStatistics->maxBytesLiveSinceReset = 0;
}

void maybeWaitForGC (GC_state s) {
  if (Proc_threadInSection (s)) {
    s->syncReason = SYNC_HELP;
    performSharedGC (s, 0);
  }
}

void Parallel_maybeWaitForGC (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  maybeWaitForGC (s);
}



//struct rusage ru_lock;

void Parallel_lock (Int32 p) {
  GC_state s = pthread_getspecific (gcstate_key);
  int32_t myNumber = Proc_processorNumber (s);

  //fprintf (stderr, "lock\n");

  /*
  if (needGCTime (s))
    startTiming (&ru_lock);
  */

  do {
  AGAIN:
    Parallel_maybeWaitForGC ();
    if (Parallel_mutexes[p] >= 0)
      goto AGAIN;
  } while (not __sync_bool_compare_and_swap (&Parallel_mutexes[p],
                                             -1,
                                             myNumber));
  /*
  if (needGCTime (s))
    stopTiming (&ru_lock, &s->cumulativeStatistics->ru_lock);
  */
}

void Parallel_unlock (Int32 p) {
  GC_state s = pthread_getspecific (gcstate_key);
  int32_t myNumber = Proc_processorNumber (s);

  //fprintf (stderr, "unlock %d\n", Parallel_holdingMutex);

  if (not __sync_bool_compare_and_swap (&Parallel_mutexes[p],
                                        myNumber,
                                        -1)) {
    fprintf (stderr, "can't unlock if you don't hold the lock\n");
  }
}

Int32 Parallel_fetchAndAdd (pointer p, Int32 v) {
  //fprintf (stderr, "fetchAndAdd\n");

  Int32 res = __sync_fetch_and_add ((Int32 *)p, v);
  /*
  asm volatile ("mfence");
  */
  /*
  asm volatile ("lock; xaddl %0,%1"
                : "+q" (v) // output
                : "m" (*p) // input
                : "memory"); // clobbered
  //  asm volatile ("mfence");
  */
  return res;
}

bool Parallel_compareAndSwap (pointer p, Int32 old, Int32 new) {
  return __sync_bool_compare_and_swap ((Int32 *)p, old, new);
}

Int32 Parallel_vCompareAndSwap (pointer p, Int32 old, Int32 new) {
    return __sync_val_compare_and_swap ((Int32 *)p, old, new);
}

void Parallel_enablePreemption (void)
{
  GC_state s = pthread_getspecific (gcstate_key);
  sigaddset(&s->signalsInfo.signalsHandled, SIGALRM);
}

void Parallel_disablePreemption (void)
{
  GC_state s = pthread_getspecific (gcstate_key);
  sigdelset(&s->signalsInfo.signalsHandled, SIGALRM);
  sigdelset(&s->signalsInfo.signalsPending, SIGALRM);
}

long long
timeval_diff(struct timeval *difference,
             struct timeval *end_time,
             struct timeval *start_time) {
    struct timeval temp_diff;

    if(difference==NULL)
    {
        difference=&temp_diff;
    }

    difference->tv_sec =end_time->tv_sec -start_time->tv_sec ;
    difference->tv_usec=end_time->tv_usec-start_time->tv_usec;

    /* Using while instead of if below makes the code slightly more robust. */

    while(difference->tv_usec<0)
    {
        difference->tv_usec+=1000000;
        difference->tv_sec -=1;
    }

    return 1000000LL*difference->tv_sec+
        difference->tv_usec;

} /* timeval_diff() */

void Parallel_wait (void) {
    GC_state s = pthread_getspecific (gcstate_key);
    int p = Parallel_processorNumber ();
    sigset_t set;
    sigemptyset (&set);
    sigaddset (&set, SIGUSR2);
    //struct timeval starttime,endtime,timediff;

    //gettimeofday(&starttime,0x0);
    pthread_mutex_lock (&waitMutex[p]);
    if (!(Proc_threadInSection (s) || dataInMutatorQ[p])) {
        pthread_sigmask (SIG_BLOCK, &set, NULL);
        pthread_cond_wait (&waitCondVar[p], &waitMutex[p]);
        pthread_sigmask (SIG_UNBLOCK, &set, NULL);
    }
    dataInMutatorQ[p] = FALSE;
    pthread_mutex_unlock (&waitMutex[p]);
    //gettimeofday(&endtime,0x0);
    //long long diff = timeval_diff(&timediff,&endtime,&starttime);
    //fprintf(stderr, "Diff %lld[%d]\n", diff, Parallel_processorNumber ());
}

void Parallel_wakeUpThread (Int32 p, Int32 dataIn) {
    pthread_mutex_lock (&waitMutex[p]);
    if (dataIn == 1) dataInMutatorQ[p] = TRUE;
    pthread_cond_signal (&waitCondVar[p]);
    pthread_mutex_unlock (&waitMutex[p]);
}
