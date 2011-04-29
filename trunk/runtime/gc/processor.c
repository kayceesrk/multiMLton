
#include <pthread.h>

int32_t Proc_processorNumber (GC_state s) {
  for (int proc = 0; proc < s->numberOfProcs; proc ++) {
    if (s == &(s->procStates[proc])) {
      return (int32_t)proc;
    }
  }

  /* XXX shouldn't get here */
  fprintf (stderr, "don't know my own processor number (signals?)\n");
  exit (1);
  return 0;
}

bool Proc_amPrimary (GC_state s) {
  return Proc_processorNumber (s) == 0;
}

volatile bool Proc_beginInit = FALSE;
volatile int32_t Proc_initialized = 0;
volatile int32_t Proc_criticalCount;
volatile int32_t Proc_criticalTicket;

void Proc_waitForInitialization (GC_state s) {
  int32_t unused;

  while (!Proc_beginInit) { }

  unused = __sync_fetch_and_add (&Proc_initialized, 1);

  while (!Proc_isInitialized (s)) { }
  initProfiling (s);
}

void Proc_signalInitialization (GC_state s) {

  Proc_criticalTicket = -1;
  Proc_criticalCount = 0;

  Proc_initialized = 1;
  Proc_beginInit = TRUE;

  while (!Proc_isInitialized (s)) { }
}

bool Proc_isInitialized (GC_state s) {
  return Proc_initialized == s->numberOfProcs;
}

struct timeval tv_serial;
struct timeval tv_sync;


void Proc_beginCriticalSection (GC_state s) {
  if (Proc_isInitialized (s)) {
    int32_t myNumber = Proc_processorNumber (s);
    int32_t p = __sync_add_and_fetch (&Proc_criticalCount, 1);

    assert (s->atomicState > 0);

    if (p == 1) {
      /* We are the first thread in this round. */
      if (needGCTime (s))
        startWallTiming (&tv_sync);

      incSync (s);

      for (int i=0;i<s->numberOfProcs;i++)
        Parallel_wakeUpThread (i, 0);
    }

    if (p == s->numberOfProcs) {
      /* We are the last to syncronize */
      if (needGCTime (s)) {
        stopWallTiming (&tv_sync, &s->cumulativeStatistics->tv_sync);
        startWallTiming (&tv_serial);
      }
      Proc_criticalTicket = 0;
    }

    while (Proc_criticalTicket != myNumber) {}
  }
  else {
    Proc_criticalCount = 1;
  }
}

void Proc_endCriticalSection (__attribute__ ((unused)) GC_state s) {
  if (Proc_isInitialized (s)) {
    assert (s->atomicState > 0);
    int32_t p = __sync_add_and_fetch (&Proc_criticalTicket, 1);
    if (p == s->numberOfProcs) {
      /* We are the last to finish */

      if (needGCTime (s))
        stopWallTiming (&tv_serial, &s->cumulativeStatistics->tv_serial);

      Proc_criticalCount = 0;
      Proc_criticalTicket = -1;
      __sync_synchronize ();
    }

    while (Proc_criticalTicket >= 0) {}
  }
  else {
    Proc_criticalCount = 0;
  }
}

/* returns true if some thread has initiated a barrier and/or inside a critical
 * section */
bool Proc_threadInSection (__attribute__ ((unused)) GC_state s) {
  return Proc_criticalCount > 0;
}

/* Returns true if all threads are in a critical section and executing it. Main
 * difference from Proc_threadInSection is that here true is returned only if
 * all threads are already in critical section */
bool Proc_executingInSection (__attribute__ ((unused)) GC_state s) {
  return Proc_criticalTicket >= 0;
}
