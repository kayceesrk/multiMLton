/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void minorGC (GC_state s) {
  minorCheneyCopyGC (s);
}

void majorGC (GC_state s, size_t bytesRequested, bool mayResize) {
  uintmax_t numGCs;
  size_t desiredSize;

  s->lastMajorStatistics->numMinorGCs = 0;
  numGCs =
    s->cumulativeStatistics->numCopyingGCs
    + s->cumulativeStatistics->numMarkCompactGCs;
  if (0 < numGCs
      and ((float)(s->cumulativeStatistics->numHashConsGCs) / (float)(numGCs)
           < s->controls->ratios.hashCons))
    s->hashConsDuringGC = TRUE;
  desiredSize =
    sizeofHeapDesired (s, s->lastMajorStatistics->bytesLive + bytesRequested, 0);
  if (not FORCE_MARK_COMPACT
      and not s->hashConsDuringGC // only markCompact can hash cons
      and s->heap->size < s->sysvals.ram
      and (not isHeapInit (s->secondaryHeap)
           or createHeapSecondary (s, desiredSize)))
    majorCheneyCopyGC (s);
  else
    majorMarkCompactGC (s);
  s->hashConsDuringGC = FALSE;
  s->lastMajorStatistics->bytesLive = s->heap->oldGenSize;
  if (s->lastMajorStatistics->bytesLive > s->cumulativeStatistics->maxBytesLive)
    s->cumulativeStatistics->maxBytesLive = s->lastMajorStatistics->bytesLive;
  /* Notice that the s->lastMajorStatistics->bytesLive below is
   * different than the s->lastMajorStatistics->bytesLive used as an
   * argument to createHeapSecondary above.  Above, it was an
   * estimate.  Here, it is exactly how much was live after the GC.
   */
  if (mayResize) {
    resizeHeap (s, s->lastMajorStatistics->bytesLive + bytesRequested);
    setCardMapAndCrossMap (s);
  }
  resizeHeapSecondary (s);
  assert (s->heap->oldGenSize + bytesRequested <= s->heap->size);
}

void growStackCurrent (GC_state s, bool allocInOldGen) {
  size_t reserved;
  GC_stack stack;

  reserved = sizeofStackGrowReserved (s, getStackCurrent(s));
  if (DEBUG_STACKS or s->controls->messages)
    fprintf (stderr,
             "[GC: Growing stack of size %s bytes to size %s bytes, using %s bytes.]\n",
             uintmaxToCommaString(getStackCurrent(s)->reserved),
             uintmaxToCommaString(reserved),
             uintmaxToCommaString(getStackCurrent(s)->used));
  stack = newStack (s, reserved, allocInOldGen);
  copyStack (s, getStackCurrent(s), stack);
  getThreadCurrent(s)->stack = pointerToObjptr ((pointer)stack, s->heap->start);
}

void enterGC (GC_state s) {
  if (s->profiling.isOn) {
    /* We don't need to profileEnter for count profiling because it
     * has already bumped the counter.  If we did allow the bump, then
     * the count would look like function(s) had run an extra time.
     */
    if (s->profiling.stack
        and not (PROFILE_COUNT == s->profiling.kind))
      GC_profileEnter (s);
  }
  s->amInGC = TRUE;
}

void leaveGC (GC_state s) {
  if (s->profiling.isOn) {
    if (s->profiling.stack
        and not (PROFILE_COUNT == s->profiling.kind))
      GC_profileLeave (s);
  }
  s->amInGC = FALSE;
}

void performGC (GC_state s,
                size_t oldGenBytesRequested,
                size_t nurseryBytesRequested,
                bool forceMajor,
                bool mayResize) {
  uintmax_t gcTime;
  bool stackTopOk;
  size_t stackBytesRequested;
  struct timeval tv_start;
  size_t totalBytesRequested;

  enterGC (s);
  s->cumulativeStatistics->numGCs++;
  if (DEBUG or s->controls->messages) {
    size_t nurserySize = s->heap->size - (s->heap->nursery - s->heap->start);
    size_t nurseryUsed = s->frontier - s->heap->nursery;
    fprintf (stderr,
             "[GC: Starting gc #%s; requesting %s nursery bytes and %s old-gen bytes,]\n",
             uintmaxToCommaString(s->cumulativeStatistics->numGCs),
             uintmaxToCommaString(nurseryBytesRequested),
             uintmaxToCommaString(oldGenBytesRequested));
    fprintf (stderr,
             "[GC:\theap at "FMTPTR" of size %s bytes,]\n",
             (uintptr_t)(s->heap->start),
             uintmaxToCommaString(s->heap->size));
    fprintf (stderr,
             "[GC:\twith nursery of size %s bytes (%.1f%% of heap),]\n",
             uintmaxToCommaString(nurserySize),
             100.0 * ((double)(nurserySize) / (double)(s->heap->size)));
    fprintf (stderr,
             "[GC:\tand old-gen of size %s bytes (%.1f%% of heap),]\n",
             uintmaxToCommaString(s->heap->oldGenSize),
             100.0 * ((double)(s->heap->oldGenSize) / (double)(s->heap->size)));
    fprintf (stderr,
             "[GC:\tand nursery using %s bytes (%.1f%% of heap, %.1f%% of nursery).]\n",
             uintmaxToCommaString(nurseryUsed),
             100.0 * ((double)(nurseryUsed) / (double)(s->heap->size)),
             100.0 * ((double)(nurseryUsed) / (double)(nurserySize)));
  }
  assert (invariantForGC (s));
  if (needGCTime (s))
    startWallTiming (&tv_start);
  minorGC (s);
  stackTopOk = invariantForMutatorStack (s);
  stackBytesRequested =
    stackTopOk
    ? 0
    : sizeofStackWithHeader (s, sizeofStackGrowReserved (s, getStackCurrent (s)));
  totalBytesRequested =
    oldGenBytesRequested
    + stackBytesRequested;
  getThreadCurrent(s)->bytesNeeded = nurseryBytesRequested;
  for (int proc = 0; proc < s->numberOfProcs; proc++) {
    /* It could be that other threads have already worked to satisfy their own
       requests.  We need to make sure that we don't invalidate the work
       they've done.
    */
    if (getThreadCurrent(&s->procStates[proc])->bytesNeeded == 0) {
      getThreadCurrent(&s->procStates[proc])->bytesNeeded = GC_HEAP_LIMIT_SLOP;
    }
    totalBytesRequested += getThreadCurrent(&s->procStates[proc])->bytesNeeded;
    totalBytesRequested += GC_BONUS_SLOP;
  }

  if (forceMajor
      or totalBytesRequested > s->heap->availableSize - s->heap->oldGenSize)
    majorGC (s, totalBytesRequested, mayResize);
  setGCStateCurrentHeap (s, oldGenBytesRequested + stackBytesRequested,
                         nurseryBytesRequested, false);
  assert (hasHeapBytesFree (s, oldGenBytesRequested + stackBytesRequested,
                            nurseryBytesRequested));
  unless (stackTopOk)
    growStackCurrent (s, TRUE);
  for (int proc = 0; proc < s->numberOfProcs; proc++) {
    /* DOC XXX must come first to setup maps properly */
    s->procStates[proc].generationalMaps = s->generationalMaps;
    setGCStateCurrentThreadAndStack (&s->procStates[proc]);
  }
  if (needGCTime (s)) {
    gcTime = stopWallTiming (&tv_start, &s->cumulativeStatistics->ru_gc);
    s->cumulativeStatistics->maxPauseTime =
      max (s->cumulativeStatistics->maxPauseTime, gcTime);
  } else
    gcTime = 0;  /* Assign gcTime to quell gcc warning. */
  if (DEBUG or s->controls->messages) {
    size_t nurserySize = s->heap->size - (s->heap->nursery - s->heap->start);
    fprintf (stderr,
             "[GC: Finished gc #%s; time %s ms,]\n",
             uintmaxToCommaString(s->cumulativeStatistics->numGCs),
             uintmaxToCommaString(gcTime));
    fprintf (stderr,
             "[GC:\theap at "FMTPTR" of size %s bytes,]\n",
             (uintptr_t)(s->heap->start),
             uintmaxToCommaString(s->heap->size));
    fprintf (stderr,
             "[GC:\twith nursery of size %s bytes (%.1f%% of heap),]\n",
             uintmaxToCommaString(nurserySize),
             100.0 * ((double)(nurserySize) / (double)(s->heap->size)));
    fprintf (stderr,
             "[GC:\tand old-gen of size %s bytes (%.1f%% of heap).]\n",
             uintmaxToCommaString(s->heap->oldGenSize),
             100.0 * ((double)(s->heap->oldGenSize) / (double)(s->heap->size)));
  }
  /* Send a GC signal. */
  if (s->signalsInfo.gcSignalHandled
      and s->signalHandlerThread != BOGUS_OBJPTR) {
    if (DEBUG_SIGNALS)
      fprintf (stderr, "GC Signal pending.\n");
    s->signalsInfo.gcSignalPending = TRUE;
    unless (s->signalsInfo.amInSignalHandler)
      s->signalsInfo.signalIsPending = TRUE;
  }
  if (DEBUG)
    displayGCState (s, stderr);
  assert (hasHeapBytesFree (s, oldGenBytesRequested, nurseryBytesRequested));
  assert (invariantForGC (s));
  leaveGC (s);
}

size_t fillGap (__attribute__ ((unused)) GC_state s, pointer start, pointer end) {
  size_t diff = end - start;

  if (diff == 0) {
    return 0;
  }

  if (DEBUG)
    fprintf (stderr, "[GC: Filling gap between "FMTPTR" and "FMTPTR" (size = %zu).]\n",
             (uintptr_t)start, (uintptr_t)end, diff);

  if (start) {
    /* See note in the array case of foreach.c (line 103) */
    if (diff >= GC_ARRAY_HEADER_SIZE + OBJPTR_SIZE) {
      assert (diff >= GC_ARRAY_HEADER_SIZE);
      /* Counter */
      *((GC_arrayCounter *)start) = 0;
      start = start + GC_ARRAY_COUNTER_SIZE;
      /* Length */
      *((GC_arrayLength *)start) = diff - GC_ARRAY_HEADER_SIZE;
      start = start + GC_ARRAY_LENGTH_SIZE;
      /* Header */
      *((GC_header *)start) = GC_WORD8_VECTOR_HEADER;
      start = start + GC_HEADER_SIZE;
    }
    else if (diff == GC_HEADER_SIZE) {
      *((GC_header *)start) = GC_HEADER_ONLY_HEADER;
      start = start + GC_HEADER_SIZE;
    }
    else if (diff >= GC_BONUS_SLOP) {
      assert (diff < INT_MAX);
      *((GC_header *)start) = GC_FILL_HEADER;
      start = start + GC_HEADER_SIZE;
      *((GC_smallGapSize *)start) = diff - (GC_HEADER_SIZE + GC_SMALL_GAP_SIZE_SIZE);
      start = start + GC_SMALL_GAP_SIZE_SIZE;
    }
    else {
      assert(0 == diff);
      /* XXX */
      fprintf (stderr, "FOUND A GAP OF %zu BYTES!\n", diff);
      exit (1);
    }

    /* XXX debug only */
    /*
    while (start < end) {
      *(start++) = 0xDF;
    }
    */

    return diff;
  }
  else {
    return 0;
  }
}

// assumes that stack->used and thread->exnstack are up to date
// assumes exclusive access to runtime if !mustEnter
// forceGC = force major collection
void ensureHasHeapBytesFreeAndOrInvariantForMutator (GC_state s, bool forceGC,
                                                     bool ensureFrontier,
                                                     bool ensureStack,
                                                     size_t oldGenBytesRequested,
                                                     size_t nurseryBytesRequested,
                                                     bool fromGCCollect,
                                                     bool forceStackGrowth) {
  bool stackTopOk;
  size_t stackBytesRequested;

  assert ((int)nurseryBytesRequested >=0 );
  /* To ensure the mutator frontier invariant, set the requested bytes
     to include those needed by the thread.
   */
  if (ensureFrontier) {
    nurseryBytesRequested += getThreadCurrent(s)->bytesNeeded;
  }

  /* XXX (sort of) copied from performGC */
  stackTopOk = (not forceStackGrowth) and ((not ensureStack) or invariantForMutatorStack (s));
  stackBytesRequested =
    stackTopOk
    ? 0
    : sizeofStackWithHeader (s, sizeofStackGrowReserved (s, getStackCurrent (s)));

  if (forceStackGrowth && DEBUG_SPLICE)
      fprintf (stderr, "stackBytesRequested = %ld\n", stackBytesRequested);

  if (not stackTopOk) {
    if (DEBUG or s->controls->messages)
      fprintf (stderr, "GC: growing stack locally... [%d]\n",
               s->procStates ? Proc_processorNumber (s) : -1);
    growStackCurrent (s, FALSE);
    setGCStateCurrentThreadAndStack (s);
  }

  if (DEBUG or s->controls->messages) {
    fprintf (stderr, "GC: stackInvariant: %d,%d hasHeapBytesFree: %d inSection: %d force: %d [%d]\n",
             ensureStack, ensureStack and invariantForMutatorStack (s),
             hasHeapBytesFree (s, oldGenBytesRequested, nurseryBytesRequested),
             Proc_threadInSection (s),
             forceGC,
             s->procStates ? Proc_processorNumber (s) : -1);
  }

  if ( /* we have signals pending */
      (s->signalsInfo.signalIsPending
           and (s->syncReason = SYNC_SIGNALS))
      /* check the stack of the current thread */
      or ((ensureStack and not invariantForMutatorStack (s))
          and (s->syncReason = SYNC_STACK))
      /* this subsumes invariantForMutatorFrontier */
      or (not hasHeapBytesFree (s, oldGenBytesRequested, nurseryBytesRequested)
            and (s->syncReason = SYNC_HEAP))
      /* another thread is waiting for exclusive access */
      or Proc_threadInSection (s)
      /* we are forcing a major collection */
      or (forceGC
           and (s->syncReason = SYNC_FORCE))
      ) {

    /* Copy the value here so other threads will see it (if we synchronize and
       one of the other threads does the work). */
    if (isObjptr (getThreadCurrentObjptr(s)))
      getThreadCurrent(s)->bytesNeeded = nurseryBytesRequested;

    ENTER0 (s);
    if (fromGCCollect and (not s->signalsInfo.amInSignalHandler))
        switchToSignalHandlerThreadIfNonAtomicAndSignalPending (s);
    if ((ensureStack and not invariantForMutatorStack (s))
        or not hasHeapBytesFree (s, oldGenBytesRequested, nurseryBytesRequested)
        or forceGC) {
      performGC (s, oldGenBytesRequested, nurseryBytesRequested, forceGC, TRUE);
    }
    else
      if (DEBUG or s->controls->messages)
        fprintf (stderr, "GC: Skipping GC (inside of sync). [%d]\n", s->procStates ? Proc_processorNumber (s) : -1);

    LEAVE0 (s);
  }
  else {
    if (DEBUG or s->controls->messages)
      fprintf (stderr, "GC: Skipping GC (invariants already hold / request satisfied locally). [%d]\n", s->procStates ? Proc_processorNumber (s) : -1);

    /* These are safe even without ENTER/LEAVE */
    assert (isAligned (s->heap->size, s->sysvals.pageSize));
    assert (isAligned ((size_t)s->heap->start, CARD_SIZE));
    assert (isFrontierAligned (s, s->heap->start + s->heap->oldGenSize));
    assert (isFrontierAligned (s, s->heap->nursery));
    assert (isFrontierAligned (s, s->frontier));
    assert (s->heap->start + s->heap->oldGenSize <= s->heap->nursery);
    assert (s->heap->nursery <= s->heap->start + s->heap->availableSize);
    assert (s->heap->nursery <= s->frontier or 0 == s->frontier);
    assert (s->start <= s->frontier);
    unless (0 == s->heap->size or 0 == s->frontier) {
      assert (s->frontier <= s->limitPlusSlop);
      assert ((s->limit == 0) or (s->limit == s->limitPlusSlop - GC_HEAP_LIMIT_SLOP));
      assert (hasHeapBytesFree (s, 0, 0));
    }
  }
  assert (not ensureFrontier or invariantForMutatorFrontier(s));
  assert (not ensureStack or invariantForMutatorStack(s));
}

void GC_collect (GC_state s, size_t bytesRequested, bool force,
                 char *file, int line) {
  if (DEBUG or s->controls->messages)
    fprintf (stderr, "%s %d: GC_collect [%d]\n", file, line,
             Proc_processorNumber (s));

  /* XXX copied from enter() */
  /* used needs to be set because the mutator has changed s->stackTop. */
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  getThreadCurrent(s)->bytesNeeded = bytesRequested;


  ensureHasHeapBytesFreeAndOrInvariantForMutator (s, force,
                                                  FALSE, TRUE,
                                                  0, 0, TRUE, FALSE);
}

pointer FFI_getOpArgsResPtr (GC_state s) {
  return s->ffiOpArgsResPtr;
}
