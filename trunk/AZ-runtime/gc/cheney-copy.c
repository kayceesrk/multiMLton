/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                    Cheney Copying Collection                     */
/* ---------------------------------------------------------------- */

extern bool skipStackToThreadTracing;

void updateWeaksForCheneyCopy (GC_state s) {
  pointer p;
  GC_weak w;

  for (w = s->weaks; w != NULL; w = w->link) {

    if (DEBUG_WEAK)
      fprintf (stderr, "updateWeaksForCheneyCopy  w = "FMTPTR"  ", (uintptr_t)w);

    if (BOGUS_OBJPTR == w->objptr) {
      if (DEBUG_WEAK)
        fprintf (stderr, "already cleared\n");
      continue;
    }

    p = objptrToPointer (w->objptr, s->heap->start);
    if (GC_FORWARDED == getHeader (p)) {
      if (DEBUG_WEAK)
        fprintf (stderr, "forwarded from "FMTOBJPTR" to "FMTOBJPTR"\n",
                 w->objptr,
                 *(objptr*)p);
      w->objptr = *(objptr*)p;
    } else {
      if (DEBUG_WEAK)
        fprintf (stderr, "cleared\n");
      *(getHeaderp((pointer)w - offsetofWeak (s))) = GC_WEAK_GONE_HEADER;
      w->objptr = BOGUS_OBJPTR;
    }
  }
  s->weaks = NULL;
}

void swapHeapsForCheneyCopy (GC_state s) {
  GC_heap tempHeap;
  tempHeap = s->secondaryLocalHeap;
  s->secondaryLocalHeap = s->heap;
  s->heap = tempHeap;
  setCardMapAbsolute (s);
}

void swapHeapsForSharedCheneyCopy (GC_state s) {
  GC_heap tempHeap;
  for (int proc = 0; proc < s->numberOfProcs; proc++) {
    tempHeap = s->procStates[proc].secondarySharedHeap;
    s->procStates[proc].secondarySharedHeap =
        s->procStates[proc].sharedHeap;
    s->procStates[proc].sharedHeap = tempHeap;
  }
}

void majorCheneyCopyGC (GC_state s) {
  size_t bytesCopied;
  struct rusage ru_start;
  pointer toStart;

  assert (s->secondaryLocalHeap->size >= s->heap->oldGenSize);
  if (detailedGCTime (s))
    startTiming (&ru_start);
  s->cumulativeStatistics->numCopyingGCs++;
  s->forwardState.amInMinorGC = FALSE;
  if (DEBUG or s->controls->messages) {
    fprintf (stderr,
             "[GC: Starting major Cheney-copy] [%d]\n", s->procId);
    fprintf (stderr,
             "[GC:\tfrom heap at "FMTPTR" of size %s bytes,] [%d]\n",
             (uintptr_t)(s->heap->start),
             uintmaxToCommaString(s->heap->size), s->procId);
    fprintf (stderr,
             "[GC:\tto heap at "FMTPTR" of size %s bytes.] [%d]\n",
             (uintptr_t)(s->secondaryLocalHeap->start),
             uintmaxToCommaString(s->secondaryLocalHeap->size), s->procId);
  }
  s->forwardState.toStart = s->secondaryLocalHeap->start;
  s->forwardState.toLimit = s->secondaryLocalHeap->start + s->secondaryLocalHeap->size;
  s->forwardState.forceStackForwarding = FALSE;
  assert (s->secondaryLocalHeap->start != (pointer)NULL);
  /* The next assert ensures there is enough space for the copy to
   * succeed.  It does not assert
   *   (s->secondaryLocalHeap->size >= s->heap->size)
   * because that is too strong.
   */
  assert (s->secondaryLocalHeap->size >= s->heap->oldGenSize);
  toStart = alignFrontier (s, s->secondaryLocalHeap->start);
  s->forwardState.back = toStart;
  foreachGlobalObjptrInScope (s, forwardObjptrIfInLocalHeap);
  foreachObjptrInRange (s, toStart, &s->forwardState.back, forwardObjptrIfInLocalHeap, TRUE);
  updateWeaksForCheneyCopy (s);
  s->secondaryLocalHeap->oldGenSize = s->forwardState.back - s->secondaryLocalHeap->start;
  bytesCopied = s->secondaryLocalHeap->oldGenSize;
  s->cumulativeStatistics->bytesCopied += bytesCopied;
  swapHeapsForCheneyCopy (s);
  s->lastMajorStatistics->kind = GC_COPYING;
  if (detailedGCTime (s))
    stopTiming (&ru_start, &s->cumulativeStatistics->ru_gcCopying);
  if (DEBUG or s->controls->messages)
    fprintf (stderr,
             "[GC: Finished major Cheney-copy; copied %s bytes.] [%d]\n",
             uintmaxToCommaString(bytesCopied), s->procId);
}

void majorCheneyCopySharedGC (GC_state s) {
  size_t bytesCopied;
  struct rusage ru_start;
  pointer toStart;

  assert (s->secondarySharedHeap->size >= s->sharedHeap->oldGenSize);
  if (detailedGCTime (s))
    startTiming (&ru_start);
  s->cumulativeStatistics->numCopyingSharedGCs++;
  s->forwardState.amInMinorGC = FALSE;
  if (DEBUG or s->controls->messages) {
    fprintf (stderr,
             "[GC: Starting shared major Cheney-copy] [%d]\n", s->procId);
    fprintf (stderr,
             "[GC:\tfrom heap at "FMTPTR" of size %s bytes,] [%d]\n",
             (uintptr_t)(s->sharedHeap->start),
             uintmaxToCommaString(s->sharedHeap->size), s->procId);
    fprintf (stderr,
             "[GC:\tto heap at "FMTPTR" of size %s bytes.] [%d]\n",
             (uintptr_t)(s->secondarySharedHeap->start),
             uintmaxToCommaString(s->secondarySharedHeap->size), s->procId);
  }

  //Set up forwarding state
  toStart = alignFrontier (s, s->secondarySharedHeap->start);
  for (int proc=0; proc < s->numberOfProcs; proc++) {
    s->procStates[proc].forwardState.toStart = s->secondarySharedHeap->start;
    s->procStates[proc].forwardState.toLimit =
      s->secondarySharedHeap->start + s->secondarySharedHeap->size;
    s->procStates[proc].forwardState.back = toStart;
    s->procStates[proc].forwardState.forceStackForwarding = TRUE;

    assert (!s->procStates[proc].forwardState.rangeListCurrent);
    assert (!s->procStates[proc].forwardState.rangeListFirst);
    assert (!s->procStates[proc].forwardState.rangeListLast);
  }
  assert (s->secondarySharedHeap->start);
  assert (s->secondarySharedHeap->size >= s->sharedHeap->oldGenSize);

  // Walk the local heaps and forward objptrs
  for (int proc=0; proc < s->numberOfProcs; proc++) {
    GC_state r = &(s->procStates[proc]);
    if (DEBUG_DETAILED)
      fprintf (stderr, "majorCheneyCopySharedGC: walking local heaps (1) [%d]\n",
               proc);
    pointer end = r->heap->start + r->heap->oldGenSize;

    /* See foreachObjptrInObject():STACK for the details of skipStackToThreadTracing variable */
    skipStackToThreadTracing = TRUE;
    foreachObjptrInRangeWithFill (r, r->heap->start, &end, forwardObjptrForSharedCheneyCopy, TRUE, TRUE);
    if (r->frontier > r->heap->start + r->heap->oldGenSize)
      foreachObjptrInRangeWithFill (r, r->heap->nursery, &r->frontier, forwardObjptrForSharedCheneyCopy, TRUE, TRUE);
    skipStackToThreadTracing = FALSE;

    if (DEBUG_DETAILED)
      fprintf (stderr, "majorCheneyCopySharedGC: walking local heaps (2) [%d]\n",
               proc);
    callIfIsObjptr (r, forwardObjptrForSharedCheneyCopy, &r->forwardState.liftingObject);
    pointer back = r->forwardState.back;
    for (int i=0; i < s->numberOfProcs; i++)
      s->procStates[i].forwardState.back = back;
  }

  //Forward Globals -- must come after walking local heaps for correct forwarding state
  foreachGlobalObjptr (s, forwardObjptrForSharedCheneyCopy);

  if (DEBUG_DETAILED)
    fprintf (stderr, "majorCheneyCopySharedGC: walking to space (1) [%d]\n",
              s->procId);
  foreachObjptrInRange (s, toStart, &s->forwardState.back, forwardObjptrForSharedCheneyCopy, TRUE);
  if (DEBUG_DETAILED)
    fprintf (stderr, "majorCheneyCopySharedGC: walking to space (2) [%d]\n",
              s->procId);

  updateWeaksForCheneyCopy (s);
  s->secondarySharedHeap->oldGenSize = s->forwardState.back - s->secondarySharedHeap->start;
  bytesCopied = s->secondarySharedHeap->oldGenSize;
  s->cumulativeStatistics->bytesCopiedShared += bytesCopied;
  swapHeapsForSharedCheneyCopy (s);
  s->lastSharedMajorStatistics->kind = GC_COPYING;
  if (detailedGCTime (s))
    stopTiming (&ru_start, &s->cumulativeStatistics->ru_gcCopyingShared);

  #if 0
  fprintf (stderr, "DEBUG MODE CHECK\n");
  if (DEBUG)
    fprintf (stderr, "Starting shared heap checks [%d]\n", s->procId);

  pointer end = s->sharedHeap->start + s->sharedHeap->oldGenSize;
  foreachObjptrInRange (s, s->sharedHeap->start, &end, assertLiftedObjptr, TRUE);

  if (DEBUG)
    fprintf (stderr, "Ending shared heap checks [%d]\n", s->procId);

  for (int proc=0; proc < s->numberOfProcs; proc++) {
    if (DEBUG)
      fprintf (stderr, "Starting local heap %d check [%d]\n", proc, s->procId);

    GC_state r = &s->procStates[proc];
    end = r->heap->start + r->heap->oldGenSize;
    foreachObjptrInRange (r, r->heap->start, &end, assertIsObjptrInFromSpaceOrLifted, TRUE);
    foreachObjptrInRange (r, r->heap->nursery, &r->frontier, assertIsObjptrInFromSpaceOrLifted, TRUE);

    if (DEBUG)
      fprintf (stderr, "Ending local heap %d check [%d]\n", proc, s->procId);
  }
  #endif

  if (DEBUG or s->controls->messages)
    fprintf (stderr,
             "[GC: Finished shared major Cheney-copy; copied %s bytes.] [%d]\n",
             uintmaxToCommaString(bytesCopied), s->procId);
}


/* ---------------------------------------------------------------- */
/*                 Minor Cheney Copying Collection                  */
/* ---------------------------------------------------------------- */

void minorCheneyCopyGC (GC_state s) {
  size_t bytesAllocated;
  size_t bytesFilled = 0;
  size_t bytesCopied;
  struct rusage ru_start;

  if (DEBUG_GENERATIONAL)
    fprintf (stderr, "minorGC  nursery = "FMTPTR"  frontier = "FMTPTR"\n",
             (uintptr_t)s->heap->nursery, (uintptr_t)s->frontier);

  bytesAllocated = s->frontier - s->heap->nursery;
  if (bytesAllocated == 0)
    return;
  if (not s->canMinor) {
    if (s->limitPlusSlop != s->heap->frontier) {
      /* Fill to avoid an uninitialized gap in the middle of the heap */
      bytesFilled += fillGap (s, s->frontier, s->limitPlusSlop);
    }
    else {
      /* If this is at the end of the heap there is no need to fill the gap
         -- there will be no break in the initialized portion of the
         heap.  Also, this is the last chunk allocated in the nursery, so it is
         safe to use the frontier from this processor as the global frontier.  */
      s->heap->oldGenSize = s->frontier - s->heap->start;
    }
    bytesCopied = 0;
  } else {
    if (detailedGCTime (s))
      startTiming (&ru_start);
    s->cumulativeStatistics->numMinorGCs++;
    s->forwardState.amInMinorGC = TRUE;
    if (DEBUG_GENERATIONAL or s->controls->messages) {
      fprintf (stderr,
               "[GC: Starting minor Cheney-copy;]\n");
      fprintf (stderr,
               "[GC:\tfrom nursery at "FMTPTR" of size %s bytes.]\n",
               (uintptr_t)(s->heap->nursery),
               uintmaxToCommaString(bytesAllocated));
    }
    s->forwardState.toStart = s->heap->start + s->heap->oldGenSize;
    assert (isFrontierAligned (s, s->forwardState.toStart));
    s->forwardState.toLimit = s->forwardState.toStart + bytesAllocated;
    s->forwardState.forceStackForwarding = FALSE;
    s->forwardState.back = s->forwardState.toStart;
    /* Forward all globals.  Would like to avoid doing this once all
     * the globals have been assigned.
     */
    foreachGlobalObjptrInScope (s, forwardObjptrIfInNursery);
    forwardInterGenerationalObjptrs (s);
    foreachObjptrInRange (s, s->forwardState.toStart, &s->forwardState.back,
                          forwardObjptrIfInNursery, TRUE);
    updateWeaksForCheneyCopy (s);
    bytesCopied = s->forwardState.back - s->forwardState.toStart;
    s->cumulativeStatistics->bytesCopiedMinor += bytesCopied;
    s->heap->oldGenSize += bytesCopied;
    s->lastMajorStatistics->numMinorGCs++;
    if (detailedGCTime (s))
      stopTiming (&ru_start, &s->cumulativeStatistics->ru_gcMinor);
    if (DEBUG_GENERATIONAL or s->controls->messages)
      fprintf (stderr,
               "[GC: Finished minor Cheney-copy; copied %s bytes.]\n",
               uintmaxToCommaString(bytesCopied));
  }
  bytesAllocated -= bytesFilled;
  s->cumulativeStatistics->bytesAllocated += bytesAllocated;
  s->cumulativeStatistics->bytesFilled += bytesFilled;
}
