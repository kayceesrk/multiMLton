/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

GC_state getGCStateFromPointer (GC_state s, pointer p) {
  for (int proc=0; proc < s->numberOfProcs; proc++) {
    GC_state r = &s->procStates[proc];
    if (isPointerInHeap (r, r->heap, p))
      return r;
  }
  assert (0 and "getGCStateFromPointer: pointer location unknown");
  die ("getGCStateFromPointer: pointer location unknown");
}

void displayGCState (GC_state s, FILE *stream) {
  fprintf (stream,
           "GC state\n");
  fprintf (stream, "\tcurrentThread = "FMTOBJPTR"\n", s->currentThread);
  displayThread (s, (GC_thread)(objptrToPointer (s->currentThread, s->heap->start)
                                + offsetofThread (s)),
                 stream);
  fprintf (stream, "\tgenerational\n");
  displayGenerationalMaps (s, &s->generationalMaps,
                           stream);
  fprintf (stream, "\theap\n");
  displayHeap (s, s->heap,
               stream);
  fprintf (stream,
           "\tstart = "FMTPTR"\n"
           "\tfrontier = "FMTPTR"\n"
           "\tlimit = "FMTPTR"\n"
           "\tlimitPlusSlop = "FMTPTR"\n"
           "\tstackBottom = "FMTPTR"\n"
           "\tstackTop = "FMTPTR"\n",
           (uintptr_t)s->start,
           (uintptr_t)s->frontier,
           (uintptr_t)s->limit,
           (uintptr_t)s->limitPlusSlop,
           (uintptr_t)s->stackBottom,
           (uintptr_t)s->stackTop);
}

size_t sizeofGCStateCurrentStackUsed (GC_state s) {
  return s->stackTop - s->stackBottom;
}

void setGCStateCurrentThreadAndStack (GC_state s) {
  GC_thread thread;
  GC_stack stack;

  thread = getThreadCurrent (s);
  s->exnStack = thread->exnStack;
  stack = getStackCurrent (s);
  assert (isPointerInHeap (s, s->heap, (pointer)stack));
  s->stackBottom = getStackBottom (s, stack);
  s->stackTop = getStackTop (s, stack);
  s->stackLimit = getStackLimit (s, stack);
  if (DEBUG_DETAILED)
    fprintf (stderr, "setGCStateCurrentThreadAndStack: thread = "FMTPTR" stack = "FMTPTR" [%d]\n",
             (uintptr_t)thread, (uintptr_t)stack, s->procId);
  markCard (s, (pointer)stack);
}

void setGCStateCurrentLocalHeap (GC_state s,
                                 size_t oldGenBytesRequested,
                                 size_t nurseryBytesRequested) {
  GC_heap h;
  pointer nursery;
  size_t nurserySize;
  pointer genNursery;
  size_t genNurserySize;

  if (DEBUG_DETAILED)
    fprintf (stderr, "setGCStateCurrentLocalHeap(%s, %s)\n",
             uintmaxToCommaString(oldGenBytesRequested),
             uintmaxToCommaString(nurseryBytesRequested));
  h = s->heap;
  assert (isFrontierAligned (s, h->start + h->oldGenSize + oldGenBytesRequested));
  s->localHeapStart = h->start;
  s->limitPlusSlop = h->start + h->size;
  h->availableSize = h->size;
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
  nurserySize = h->size - (h->oldGenSize + oldGenBytesRequested);
  assert (isFrontierAligned (s, s->limitPlusSlop - nurserySize));
  nursery = s->limitPlusSlop - nurserySize;
  genNursery = alignFrontier (s, s->limitPlusSlop - (nurserySize / 2));
  genNurserySize = s->limitPlusSlop - genNursery;
  if (/* The mutator marks cards. */
      s->mutatorMarksCards
      /* There is enough space in the generational nursery. */
      and (nurseryBytesRequested <= genNurserySize)
      /* The nursery is large enough to be worth it. */
      and (((float)(h->size - s->lastMajorStatistics->bytesLive)
            / (float)nurserySize)
           <= s->controls->ratios.nursery)
      and /* There is a reason to use generational GC. */
      (
       /* We must use it for debugging purposes. */
       FORCE_GENERATIONAL
       /* We just did a mark compact, so it will be advantageous to to use it. */
       or (s->lastMajorStatistics->kind == GC_MARK_COMPACT)
       /* The live ratio is low enough to make it worthwhile. */
       or ((float)h->size / (float)s->lastMajorStatistics->bytesLive
           <= (h->withMapsSize < s->sysvals.ram
               ? s->controls->ratios.copyGenerational
               : s->controls->ratios.markCompactGenerational))
      )) {
    s->canMinor = TRUE;
    nursery = genNursery;
    nurserySize = genNurserySize;
    clearCardMap (s);
  } else {
    unless (nurseryBytesRequested <= nurserySize)
      die ("Out of memory.  Insufficient space in local nursery.");
    s->canMinor = FALSE;
  }
  assert (nurseryBytesRequested <= nurserySize);
  s->heap->nursery = nursery;
  s->frontier = s->sessionStart = nursery;
  s->start = nursery;
  h->frontier = s->limitPlusSlop;
  assert (s->heap->start + s->heap->oldGenSize <= s->heap->nursery);
  assert (nurseryBytesRequested <= (size_t)(s->limitPlusSlop - s->frontier));
  assert (isFrontierAligned (s, s->heap->nursery));
  assert (hasHeapBytesFree (s, s->heap, oldGenBytesRequested, nurseryBytesRequested));
}

void setGCStateCurrentSharedHeap (GC_state s,
                                  size_t oldGenBytesRequested,
                                  size_t nurseryBytesRequested,
                                  bool duringInit) {
  GC_heap h;
  pointer nursery;
  size_t nurserySize;
  pointer genNursery;
  size_t genNurserySize;
  pointer limit;
  pointer frontier;
  size_t bonus = GC_BONUS_SLOP * s->numberOfProcs;

  if (DEBUG_DETAILED)
    fprintf (stderr, "setGCStateCurrentSharedHeap(%s, %s)\n",
             uintmaxToCommaString(oldGenBytesRequested),
             uintmaxToCommaString(nurseryBytesRequested));
  h = s->sharedHeap;
  assert (h==s->sharedHeap);
  assert (isFrontierAligned (s, h->start + h->oldGenSize + oldGenBytesRequested));
  limit = h->start + h->size - bonus;
  nurserySize = h->size - (h->oldGenSize + oldGenBytesRequested) - bonus;
  assert (isFrontierAligned (s, limit - nurserySize));
  nursery = limit - nurserySize;
  genNursery = alignFrontier (s, limit - (nurserySize / 2));
  genNurserySize = limit - genNursery;

  if (s->controls->restrictAvailableSize
      and
      (s->cumulativeStatistics->maxBytesLiveSinceReset > 0)) {
    float actualRatio;
    h->availableSize =
      (size_t)(s->controls->ratios.available
               * s->cumulativeStatistics->maxBytesLiveSinceReset);

    if ((h->oldGenSize + oldGenBytesRequested + nurserySize + bonus)
        > h->availableSize) {
      /* Limit allocation in this round */
      if ((h->oldGenSize + oldGenBytesRequested + nurseryBytesRequested + bonus)
          > h->availableSize) {
        /* We can't limit as much as we'd like, so offer enough space to
           satisfy the current request. */
        h->availableSize = h->oldGenSize + oldGenBytesRequested
          + nurseryBytesRequested + bonus;
      }
      if (h->availableSize > h->size) {
        /* Can't offer more than we have. */
        h->availableSize = h->size;
      }
      limit = h->start + h->availableSize - bonus;
      nurserySize = h->availableSize - (h->oldGenSize + oldGenBytesRequested) - bonus;
      assert (isFrontierAligned (s, limit - nurserySize));
      nursery = limit - nurserySize;
    }
    else {
      /* No need to limit in this round... reset availableSize. */
      h->availableSize = h->size;
    }

    actualRatio = (float)h->availableSize
      / s->cumulativeStatistics->maxBytesLiveSinceReset;
    if ((DEBUG or s->controls->messages)
        and
        (actualRatio > s->controls->ratios.available)) {
      fprintf (stderr,
               "[GC: Can't restrict available ratio to %f, using %f; worst-case max-live is %s bytes.]\n",
               s->controls->ratios.available, actualRatio,
               uintmaxToCommaString(h->oldGenSize + oldGenBytesRequested + nurserySize));
    }
  }
  else {
    /* Otherwise, make all unused space available */
    h->availableSize = h->size;
  }

  assert (nurseryBytesRequested <= nurserySize);
  s->sharedHeap->nursery = nursery;
  frontier = nursery;

  if (not duringInit) {
    for (int proc = 0; proc < s->numberOfProcs; proc++) {
      assert (isFrontierAligned (s, frontier));
      s->procStates[proc].sharedStart = s->procStates[proc].sharedFrontier = frontier;
      s->procStates[proc].sharedLimitPlusSlop = s->procStates[proc].sharedStart +
        getThreadCurrent(&s->procStates[proc])->bytesNeeded;
      s->procStates[proc].sharedLimit = s->procStates[proc].sharedLimitPlusSlop - GC_HEAP_LIMIT_SLOP;
      assert (s->procStates[proc].sharedFrontier <= s->procStates[proc].sharedLimitPlusSlop);
      /* XXX clearCardMap (?) */

      if (DEBUG)
        for (size_t i = 0; i < GC_BONUS_SLOP; i++)
          *(s->procStates[proc].sharedLimitPlusSlop + i) = 0xBF;

      frontier = s->procStates[proc].sharedLimitPlusSlop + GC_BONUS_SLOP;
    }
  }
  else {
    assert (Proc_processorNumber (s) == 0);
    /* XXX this is a lot of copy-paste */
    for (int proc = 0; proc < s->numberOfProcs; proc++) {
      assert (isFrontierAligned (s, frontier));
      s->procStates[proc].sharedStart = s->procStates[proc].sharedFrontier = frontier;
      s->procStates[proc].sharedLimitPlusSlop = s->procStates[proc].sharedStart +
        GC_HEAP_LIMIT_SLOP;
      s->procStates[proc].sharedLimit = s->procStates[proc].sharedLimitPlusSlop - GC_HEAP_LIMIT_SLOP;
      assert (s->procStates[proc].sharedFrontier <= s->procStates[proc].sharedLimitPlusSlop);
      /* XXX clearCardMap (?) */

      if (DEBUG)
        for (size_t i = 0; i < GC_BONUS_SLOP; i++)
          *(s->procStates[proc].sharedLimitPlusSlop + i) = 0xBF;

      frontier = s->procStates[proc].sharedLimitPlusSlop + GC_BONUS_SLOP;
    }
  }
  h->frontier = frontier;
  if (DEBUG)
    fprintf (stderr, "Frontier : %p h->frontier %p\n", (void*)frontier, (void*)h->frontier);
  assert (h->frontier <= h->start + h->availableSize);

  //Set sharedHeap limits in gcState
  for (int proc=0; proc < s->numberOfProcs; proc++) {
    s->procStates[proc].sharedHeapStart = s->sharedHeap->start;
    s->procStates[proc].sharedHeapEnd = s->sharedHeap->start + s->sharedHeap->size;
  }

  if (not duringInit) {
    assert (getThreadCurrent(s)->bytesNeeded <= (size_t)(s->sharedLimitPlusSlop - s->sharedFrontier));
    assert (hasHeapBytesFree (s, s->sharedHeap, oldGenBytesRequested, getThreadCurrent(s)->bytesNeeded));
  }
  else {
    assert (nurseryBytesRequested <= (size_t)(s->sharedLimitPlusSlop - s->sharedFrontier));
    assert (hasHeapBytesFree (s, s->sharedHeap, oldGenBytesRequested, nurseryBytesRequested));
  }
  assert (isFrontierAligned (s, s->sharedFrontier));
}

bool GC_getIsPCML (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  return s->enableTimer;
}

bool GC_getAmOriginal (__attribute__ ((unused)) GC_state *gs) {
  GC_state s = pthread_getspecific (gcstate_key);
  return s->amOriginal;
}

void GC_setAmOriginal (__attribute__ ((unused)) GC_state *gs, bool b) {
  GC_state s = pthread_getspecific (gcstate_key);
  s->amOriginal = b;
}

void GC_setControlsMessages (__attribute__ ((unused)) GC_state *gs, bool b) {
  GC_state s = pthread_getspecific (gcstate_key);
  s->controls->messages = b;
}

void GC_setControlsSummary (__attribute__ ((unused)) GC_state *gs, bool b) {
  GC_state s = pthread_getspecific (gcstate_key);
  s->controls->summary = b;
}

void GC_setControlsRusageMeasureGC (__attribute__ ((unused)) GC_state *gs, bool b) {
  GC_state s = pthread_getspecific (gcstate_key);
  s->controls->rusageMeasureGC = b;
}

uintmax_t GC_getCumulativeStatisticsBytesAllocated (__attribute__ ((unused)) GC_state *gs) {
  GC_state s = pthread_getspecific (gcstate_key);
  return s->cumulativeStatistics->bytesAllocated;
}

uintmax_t GC_getCumulativeStatisticsNumCopyingGCs (__attribute__ ((unused)) GC_state *gs) {
  GC_state s = pthread_getspecific (gcstate_key);
  return s->cumulativeStatistics->numCopyingGCs;
}

uintmax_t GC_getCumulativeStatisticsNumMarkCompactGCs (__attribute__ ((unused)) GC_state *gs) {
  GC_state s = pthread_getspecific (gcstate_key);
  return s->cumulativeStatistics->numMarkCompactGCs;
}

uintmax_t GC_getCumulativeStatisticsNumMinorGCs (__attribute__ ((unused)) GC_state *gs) {
  GC_state s = pthread_getspecific (gcstate_key);
  return s->cumulativeStatistics->numMinorGCs;
}

size_t GC_getCumulativeStatisticsMaxBytesLive (__attribute__ ((unused)) GC_state *gs) {
  GC_state s = pthread_getspecific (gcstate_key);
  return s->cumulativeStatistics->maxBytesLive;
}

void GC_setHashConsDuringGC (__attribute__ ((unused)) GC_state *gs, bool b) {
  GC_state s = pthread_getspecific (gcstate_key);
  s->hashConsDuringGC = b;
}

size_t GC_getLastMajorStatisticsBytesLive (__attribute__ ((unused)) GC_state *gs) {
  GC_state s = pthread_getspecific (gcstate_key);
  return s->lastMajorStatistics->bytesLive;
}


pointer GC_getCallFromCHandlerThread (__attribute__ ((unused)) GC_state *gs) {
  GC_state s = pthread_getspecific (gcstate_key);
  pointer p = objptrToPointer (s->callFromCHandlerThread, s->heap->start);
  return p;
}

void GC_setCallFromCHandlerThread (__attribute__ ((unused)) GC_state *gs,
                                   pointer p) {
  GC_state s = pthread_getspecific (gcstate_key);
  objptr op = pointerToObjptr (p, s->heap->start);
  s->callFromCHandlerThread = op;
}

pointer GC_getCurrentThread (__attribute__ ((unused)) GC_state *gs) {
  GC_state s = pthread_getspecific (gcstate_key);
  pointer p = objptrToPointer (s->currentThread, s->heap->start);
  return p;
}

pointer GC_getSavedThread (__attribute__ ((unused)) GC_state *gs) {
  GC_state s = pthread_getspecific (gcstate_key);
  pointer p = objptrToPointer (s->savedThread, s->heap->start);
  s->savedThread = BOGUS_OBJPTR;
  return p;
}

void GC_setSavedThread (__attribute__ ((unused)) GC_state *gs,
                        pointer p) {
  GC_state s = pthread_getspecific (gcstate_key);
  objptr op = pointerToObjptr (p, s->heap->start);
  s->savedThread = op;
}

void GC_setSignalHandlerThread (__attribute__ ((unused)) GC_state *gs, pointer p) {
  GC_state s = pthread_getspecific (gcstate_key);
  objptr op = pointerToObjptr (p, s->heap->start);
  s->signalHandlerThread = op;

  /* Move the object pointers in call-from-c-handler stack to the shared heap in
   * preparation for copying this stack to each processor */
  {
    GC_thread thrd = (GC_thread) objptrToPointer (s->signalHandlerThread, s->heap->start);
    pointer stk = objptrToPointer (thrd->stack, s->heap->start);
    moveEachObjptrInObject (s, stk);
  }

  /* Copy the mlton signal handler thread to all gcStates */
  for (int proc = 0; proc < s->numberOfProcs; proc++) {
    s->procStates[proc].signalHandlerThread =
      pointerToObjptr( copyThreadTo (s, &s->procStates[proc],
                                     objptrToPointer(s->signalHandlerThread,
                                                     s->heap->start)),
                       s->heap->start);
  }
}

sigset_t* GC_getSignalsHandledAddr (__attribute__ ((unused)) GC_state *gs) {
  GC_state s = pthread_getspecific (gcstate_key);
  return &(s->signalsInfo.signalsHandled);
}

sigset_t* GC_getSignalsPendingAddr (__attribute__ ((unused)) GC_state *gs) {
  GC_state s = pthread_getspecific (gcstate_key);
  return &(s->signalsInfo.signalsPending);
}

sigset_t* GC_getSignalsSet (__attribute__ ((unused)) GC_state *gs) {
  GC_state s = pthread_getspecific (gcstate_key);
  return &(s->signalsInfo.signalsSet);
}

void GC_setGCSignalHandled (__attribute__ ((unused)) GC_state *gs, bool b) {
  GC_state s = pthread_getspecific (gcstate_key);
  s->signalsInfo.gcSignalHandled = b;
}

bool GC_getGCSignalPending (__attribute__ ((unused)) GC_state *gs) {
  GC_state s = pthread_getspecific (gcstate_key);
  return (s->signalsInfo.gcSignalPending);
}

void GC_setGCSignalPending (__attribute__ ((unused)) GC_state *gs, bool b) {
  GC_state s = pthread_getspecific (gcstate_key);
  s->signalsInfo.gcSignalPending = b;
}

void GC_print (int i) {
  GC_state s = pthread_getspecific (gcstate_key);
  printf ("GC_print (%d)[%d]\n", i, s->procId);
}



pointer GC_forwardBase (const GC_state s, const pointer p) {
  unsigned long start, end;
  pointer retP;
  if (MEASURE_RB_CYCLE)
    rdtscll (start);

  if (MEASURE_RB_MISS) s->cumulativeStatistics->numRBChecks++;

  if (!isPointer (p) || p == (pointer)s->generationalMaps.cardMapAbsolute) {
    retP = p;
  }
  else if (*(GC_header*)(p - GC_HEADER_SIZE) == GC_FORWARDED) {
    if (MEASURE_RB_MISS) s->cumulativeStatistics->numRBChecksForwarded++;
    if (DEBUG)
      fprintf (stderr, "GC_forwardBase: forwarding "FMTPTR" to "FMTPTR" [%d]\n",
               (uintptr_t)p, (uintptr_t)*(pointer*)p, s->procId);
    retP = *(pointer*)p;
  }
  else {
    retP = p;
  }
  if (MEASURE_RB_CYCLE) {
    rdtscll (end);
    s->cumulativeStatistics->cyclesRB += (end-start);
  }
  return retP;
}

void GC_markCleanliness (const GC_state s, pointer target, pointer source,
                         char* file, int line) {
  if (!isPointer (source)) return;
  GC_header h = getHeader (source);
  GC_numReferences oldCount, newCount;

  oldCount = getNumReferences (h);
  if (target < s->sessionStart || target > s->limitPlusSlop)
    newCount = GLOBAL_MANY;
  else if (oldCount == ZERO)
    newCount = ONE;
  else if (oldCount == ONE || oldCount == LOCAL_MANY)
    newCount = LOCAL_MANY;
  else //if (oldCount == GLOBAL_MANY)
    newCount = GLOBAL_MANY;

  if (oldCount == newCount) return;

  if (DEBUG_CLEANLINESS)
    fprintf (stderr, "%s %d: GC_markCleanliness: target "FMTPTR" source "FMTPTR""
                     " oldCount %s newCount %s\n", file, line, (uintptr_t)target,
                     (uintptr_t)source, numReferencesToString (oldCount),
                     numReferencesToString (newCount));
  setNumReferences (getHeaderp (source), newCount);
  return;
}

void GC_commEvent (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  s->cumulativeStatistics->numComms++;
}

void GC_setSelectiveDebug (__attribute__((unused)) GC_state *gs, bool b) {
  GC_state s = pthread_getspecific (gcstate_key);
  s->selectiveDebug = b;
}
