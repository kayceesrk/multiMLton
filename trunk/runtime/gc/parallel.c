
#include <pthread.h>
#include <time.h>
#include "platform.h"

void Parallel_init (void) {
  GC_state s = pthread_getspecific (gcstate_key);

  /* Move the object pointers in call-from-c-handler stack to the shared heap in
    * preparation for copying this stack to each processor */
  {
    GC_thread thrd = (GC_thread) objptrToPointer (s->callFromCHandlerThread, s->heap->start);
    pointer stk = objptrToPointer (thrd->stack, s->heap->start);
    moveEachObjptrInObject (s, stk);
  }

  /* Set up call-back state in each worker threadi. Will match with Parallel_assist (). */
  GC_thread thrd = (GC_thread) objptrToPointer (s->callFromCHandlerThread, s->heap->start);
  GC_stack stk = (GC_stack) objptrToPointer (thrd->stack, s->heap->start);
  pointer bottom = getStackBottom (s, stk);
  RCCE_bcast ((char*)&stk->used, sizeof (size_t), 0, RCCE_COMM_WORLD);
  RCCE_bcast ((char*)bottom, stk->used, 0, RCCE_COMM_WORLD);

  /* Lift all objects from local heap of processor 0 to the shared heap. This
    * must come before waking up the processors */
  if (s->numberOfProcs != 1)
    liftAllObjectsDuringInit (s);

  /* Now wake them up! */
  Proc_signalInitialization (s);
}

/* Matches with bcasts in Parallel_init */
void Parallel_assistInit (GC_state s) {
  size_t reserved;
  RCCE_bcast ((char*)&reserved, sizeof (size_t), 0, RCCE_COMM_WORLD);
  GC_thread thrd = newThread (s, alignStackReserved (s, reserved));
  GC_stack stk = (GC_stack) objptrToPointer (thrd->stack, s->heap->start);
  pointer bottom = getStackBottom (s, stk);
  RCCE_bcast ((char*)bottom, reserved, 0, RCCE_COMM_WORLD);
  stk->used = reserved;
  s->callFromCHandlerThread = pointerToObjptr ((pointer)thrd, s->heap->start);
}

Int32 Parallel_processorNumber (void) {
  return RCCE_ue ();
}

Int32 Parallel_numberOfProcessors (void) {
  return RCCE_num_ues ();
}

Int32 Parallel_numIOThreads (void) {
  return 0;
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
    performSharedGCCollective (s, 0);
  }
  else if (Proc_mustExit (s)) {
    GC_doneAssist (s);
  }
}

void Parallel_maybeWaitForGC (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  maybeWaitForGC (s);
}

//struct rusage ru_lock;

void Parallel_lock (Int32 p) {
  /* This flush will ensure that global data read within the locked region is
   * upto date */
  RCCE_acquire_lock (p);
  RCCE_DCMflush ();
}

void Parallel_unlock (Int32 p) {
  /* This flush will ensure that global data written within the locked region is
   * flushed */
  RCCE_DCMflush ();
  RCCE_release_lock (p);
}

Int32 Parallel_fetchAndAdd (pointer p, Int32 v) {
  int coreId = Proc_addrToCoreId (p);
  Parallel_lock (coreId);
  int result = *(int*)p;
  *(int*)p = *(int*)p + v;
  Parallel_unlock (coreId);
  return result;
}

bool Parallel_compareAndSwap (pointer p, Int32 old, Int32 new) {
  int* ip = (int*)p;
  int i = *ip;
  if (i != old)
    return FALSE;

  int coreId = Proc_addrToCoreId (p);
  bool result;
  Parallel_lock (coreId);
  i = *ip;
  if (i != old)
    result = FALSE;
  else {
    *ip = new;
    result = TRUE;
  }
  Parallel_unlock (coreId);

  return result;
}

Int32 Parallel_vCompareAndSwap (pointer p, Int32 old, Int32 new) {
  int* ip = (int*)p;
  int i = *ip;
  if (i != old)
    return i;

  int coreId = Proc_addrToCoreId (p);
  int result;
  Parallel_lock (coreId);
  i = *ip;
  if (i != old)
    result = i;
  else {
    *ip = new;
    result = old;
  }
  Parallel_unlock (coreId);

  return result;
}

Int32 Unsafe_vCompareAndSwap (pointer p, Int32 old, Int32 new) {
  int* ip = (int*)p;
  int i = *ip;
  if (i != old)
    return i;

  int result;
  i = *ip;
  if (i != old)
    result = i;
  else {
    *ip = new;
    result = old;
  }
  return result;
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


void Parallel_wait () {
}

void Parallel_wakeUpThread (__attribute__((unused)) Int32 p, __attribute__((unused)) Int32 dataIn) {
}
