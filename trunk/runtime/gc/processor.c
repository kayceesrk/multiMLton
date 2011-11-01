int32_t Proc_processorNumber (__attribute__((unused)) GC_state s) {
  return RCCE_ue();
}

bool Proc_amPrimary (__attribute__((unused)) GC_state s) {
  return RCCE_ue () == 0;
}

int32_t Proc_addrToCoreId (pointer p) {
  return (((int)p & 0x000000FC) >> 2)% RCCE_num_ues ();
}

void Proc_waitForInitialization (GC_state s) {
  RCCE_barrier (&RCCE_COMM_WORLD);
  RCCE_DCMflush ();
  assert (readNeedsBarrier (s) != NOT_INITIALIZED);
}

void Proc_signalInitialization (GC_state s) {
  writeNeedsBarrier (s, DEFAULT);
  RCCE_DCMflush ();
  RCCE_barrier (&RCCE_COMM_WORLD);
}

bool Proc_isInitialized (GC_state s) {
  return (readNeedsBarrier (s) != NOT_INITIALIZED);
}

void Proc_beginCriticalSection (GC_state s) {
  writeNeedsBarrier (s, NEEDS_BARRIER);
  RCCE_DCMflush ();

  if (Proc_isInitialized (s)) {
    assert (s->atomicState > 0);
    /* This barrier ensures that all processors synchronize at this point */
    RCCE_barrier (&RCCE_COMM_WORLD);

    /* Only let the first processor proceed. The equivalent for the first
     * processor is in end critical section. */
    if (Proc_processorNumber (s) != 0) {
      RCCE_barrier (&RCCE_COMM_WORLD);
      if (DEBUG_DETAILED)
        fprintf (stderr, "Core %d: entering critical section\n",
                 Proc_processorNumber (s));
    }
    else {
      writeNeedsBarrier (s, IN_CRITICAL_SECTION);
      if (DEBUG_DETAILED)
        fprintf (stderr, "Core 0: entering critical section\n");
      RCCE_DCMflush ();
    }
  }
}

void Proc_endCriticalSection (GC_state s) {
  if (Proc_isInitialized (s)) {
    assert (s->atomicState > 0);

    if (Proc_processorNumber (s) == 0)
      writeNeedsBarrier (s, DEFAULT);

    /* All processors sync one more time before exiting */
    RCCE_DCMflush ();
    RCCE_barrier (&RCCE_COMM_WORLD);
  }
}

/* returns true if some thread has initiated a barrier and/or inside a critical
 * section */
bool Proc_threadInSection (GC_state s) {
  return (readNeedsBarrier (s) == NEEDS_BARRIER);
}

/* Returns true if all threads are in a critical section and executing it. Main
 * difference from Proc_threadInSection is that here true is returned only if
 * all threads are already in critical section */
bool Proc_executingInSection (GC_state s) {
  return (readNeedsBarrier (s) == IN_CRITICAL_SECTION);
}

bool Proc_mustExit (GC_state s) {
  return (readNeedsBarrier (s) == EXIT);
}
