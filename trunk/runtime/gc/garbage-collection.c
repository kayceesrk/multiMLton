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

void majorGC (GC_state s, size_t bytesRequested, bool mayResize, bool liftWBAs) {
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
    sizeofHeapDesired (s, s->lastMajorStatistics->bytesLive + bytesRequested,
                       0, LOCAL_HEAP);
  if (liftWBAs)
    liftAllObjptrsInMoveOnWBA (s);
  if (not FORCE_MARK_COMPACT
      and not s->hashConsDuringGC // only markCompact can hash cons
      and s->heap->size < s->sysvals.ram
      and (not isHeapInit (s->secondaryLocalHeap)
           or createHeapSecondary (s, desiredSize)))
    majorCheneyCopyGC (s);
  else
    majorMarkCompactGC (s);

  //Reclaim objects if present
  if (s->reachable) {
    reclaimObjects (s);
  }

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
    resizeHeap (s, s->heap, s->lastMajorStatistics->bytesLive + bytesRequested);
    setCardMapAndCrossMap (s);
  }
  resizeLocalHeapSecondary (s);

  assert (s->heap->oldGenSize + bytesRequested <= s->heap->size);
}

void growStackCurrent (GC_state s, bool allocInOldGen, size_t reservedNew) {
  size_t reserved;
  GC_stack stack;

  if (reservedNew > 0)
    reserved = reservedNew;
  else
    reserved = sizeofStackGrowReserved (s, getStackCurrent(s));
  assert (getStackCurrent (s)->reserved >= getStackCurrent (s)->used);
  if (DEBUG_STACKS or s->controls->messages) {
    fprintf (stderr,
             "[GC: Growing stack of size %s bytes to size %s bytes, using %s bytes.]\n",
             uintmaxToCommaString(getStackCurrent(s)->reserved),
             uintmaxToCommaString(reserved),
             uintmaxToCommaString(getStackCurrent(s)->used));
  }
  assert ((not allocInOldGen) || hasHeapBytesFree (s, s->heap, sizeofStackWithHeader (s, reserved), 0));
  stack = newStack (s, reserved, allocInOldGen, FALSE);
  copyStack (s, getStackCurrent(s), stack);
  getThreadCurrent(s)->stack = pointerToObjptr ((pointer)stack, s->heap->start);
  stack->thread = getThreadCurrentObjptr (s);
  if (DEBUG_STACKS)
    fprintf (stderr, "growStackCurrent: setting stack->thread pointer. stack="FMTPTR" and thread="FMTPTR"\n",
             (uintptr_t)stack, (uintptr_t)stack->thread);
  updateStackIfDangling (s, getStackCurrentObjptr (s), getThreadCurrent(s)->stack);
  markCard (s, objptrToPointer (getThreadCurrentObjptr(s), s->heap->start));
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

void fixForwardingPointers (GC_state s, bool mayResize) {
  uintmax_t gcTime;
  struct timeval tv_start;

  if (DEBUG)
      fprintf (stderr, "Starting fixForwardingPointers\n");

  enterGC (s);
  s->cumulativeStatistics->numGCs++;
  if (needGCTime (s))
    startWallTiming (&tv_start);
  size_t nurseryBytesRequested = GC_HEAP_LIMIT_SLOP;
  size_t totalBytesRequested = nurseryBytesRequested;

  minorCheneyCopyGC (s);
  majorGC (s, totalBytesRequested, mayResize, TRUE);
  setGCStateCurrentLocalHeap (s, 0, nurseryBytesRequested);
  assert (hasHeapBytesFree (s, s->heap, 0, nurseryBytesRequested));
  setGCStateCurrentThreadAndStack (s);

  #if ASSERT
  if (isPointerInNursery (s, s->heap, (pointer)getStackCurrent(s))) {
      assert (s->frontier > (pointer)getStackCurrent(s));
  }
  #endif

  if (needGCTime (s)) {
    gcTime = stopWallTiming (&tv_start, &s->cumulativeStatistics->ru_gc);
    s->cumulativeStatistics->maxPauseTime =
      max (s->cumulativeStatistics->maxPauseTime, gcTime);
  } else
    gcTime = 0;  /* Assign gcTime to quell gcc warning. */

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
  assert (hasHeapBytesFree (s, s->heap, 0, nurseryBytesRequested));
  assert (invariantForGC (s));
  leaveGC (s);
  if (DEBUG)
      fprintf (stderr, "Finished fixForwardingPointers\n");
}

void performSharedGC (GC_state s, size_t bytesRequested) {
  size_t bytesFilled = 0;

  s->forwardState.amInMinorGC = FALSE;
  if (s->reachable)
    utarray_free (s->reachable);
  s->reachable = NULL;

  objptr op = s->forwardState.liftingObject;

  if (DEBUG)
    fprintf(stderr, "Before performSharedGC: numDanglingStacks=%d [%d]\n",
            s->danglingStackListSize, s->procId);

  //Perform local GC before entering shared GC, if we are not in the middle of closure lifting
  if (op == BOGUS_OBJPTR) {
    if (DEBUG)
      fprintf (stderr, "performSharedGC: starting local GC [%d]\n", s->procId);

    s->syncReason = SYNC_MISC;
    ENTER_LOCAL0 (s);
    minorCheneyCopyGC (s);
    majorGC (s, GC_HEAP_LIMIT_SLOP, TRUE, FALSE);
    setGCStateCurrentLocalHeap (s, 0, GC_HEAP_LIMIT_SLOP);
    setGCStateCurrentThreadAndStack (s);
    LEAVE_LOCAL0 (s);

    if (DEBUG)
      fprintf (stderr, "performSharedGC: finished local GC [%d]\n", s->procId);
  }
  else { //XXX KC can the following walks be avoided, or atleast made cheaper??
    if (DEBUG)
      fprintf (stderr, "performSharedGC: starting fixing forwarding pointers [%d]\n", s->procId);

    //Fix up just the forwarding pointers
    foreachGlobalObjptrInScope (s, fixFwdObjptr);
    //Fix forwarding pointers in local heapsA
    pointer end = s->heap->start + s->heap->oldGenSize;
    foreachObjptrInRange (s, s->heap->start, &end, fixFwdObjptr, TRUE);
    if (s->frontier > end)
      foreachObjptrInRange (s, s->heap->nursery, &s->frontier, fixFwdObjptr, TRUE);
    //Fix forwarding pointers in forwarded range
    s->forwardState.rangeListCurrent = s->forwardState.rangeListFirst;
    foreachObjptrInRange (s, s->forwardState.toStart, &s->forwardState.back, fixFwdObjptr, TRUE);
    clearRangeList (s);

    if (DEBUG)
      fprintf (stderr, "performSharedGC: finished fixing forwarding pointers [%d]\n", s->procId);
  }


  /* If we are not the last processor to sync, then someone else has to know
   * about our request */
  getThreadCurrent(s)->bytesNeeded = bytesRequested;
  s->syncReason = SYNC_HEAP;
  enterGC (s);
  ENTER0 (s);

  bytesRequested = 0;
  for (int proc=0; proc < s->numberOfProcs; proc++)
    bytesRequested += getThreadCurrent(&s->procStates[proc])->bytesNeeded;

  size_t availableBytes =
    (size_t)(s->sharedHeap->start + s->sharedHeap->availableSize - s->sharedHeap->frontier);

  /* See if a GC has already been performed */
  if (bytesRequested > availableBytes) {
    /* perform GC */
    //Clear remembered stacks
    for (int proc=0; proc < s->numberOfProcs; proc++)
      clearDanglingStackList (&s->procStates[proc]);
    bytesRequested = (s->controls->allocChunkSize + GC_BONUS_SLOP) * s->numberOfProcs;

    if (DEBUG or s->controls->messages) {
        size_t nurserySize = s->sharedHeap->size - (s->sharedHeap->nursery - s->sharedHeap->start);
        size_t nurseryUsed = s->sharedFrontier - s->sharedHeap->nursery;
        fprintf (stderr,
                "[GC: Starting shared heap gc #%s; requesting %s bytes,]\n",
                uintmaxToCommaString(s->cumulativeStatistics->numCopyingSharedGCs +
                                     s->cumulativeStatistics->numMarkCompactSharedGCs + 1),
                uintmaxToCommaString(bytesRequested));
        fprintf (stderr,
                "[GC:\tshared heap at "FMTPTR" of size %s bytes,]\n",
                (uintptr_t)(s->sharedHeap->start),
                uintmaxToCommaString(s->sharedHeap->size));
        fprintf (stderr,
                "[GC:\twith nursery of size %s bytes (%.1f%% of heap),]\n",
                uintmaxToCommaString(nurserySize),
                100.0 * ((double)(nurserySize) / (double)(s->sharedHeap->size)));
        fprintf (stderr,
                "[GC:\tand old-gen of size %s bytes (%.1f%% of heap),]\n",
                uintmaxToCommaString(s->sharedHeap->oldGenSize),
                100.0 * ((double)(s->sharedHeap->oldGenSize) / (double)(s->sharedHeap->size)));
        fprintf (stderr,
                "[GC:\tand nursery using %s bytes (%.1f%% of heap, %.1f%% of nursery).]\n",
                uintmaxToCommaString(nurseryUsed),
                100.0 * ((double)(nurseryUsed) / (double)(s->sharedHeap->size)),
                100.0 * ((double)(nurseryUsed) / (double)(nurserySize)));
    }

    for (int proc = 0; proc < s->numberOfProcs; proc++) {
      /* Add in the bonus slop now since we need to fill it */
      s->procStates[proc].sharedLimitPlusSlop += GC_BONUS_SLOP;
      if (s->procStates[proc].sharedLimitPlusSlop != s->sharedHeap->frontier) {
        /* Fill to avoid an uninitialized gap in the middle of the heap */
        bytesFilled += fillGap (s, s->procStates[proc].sharedFrontier,
                                s->procStates[proc].sharedLimitPlusSlop);
      }
      else {
        /* If this is at the end of the heap there is no need to fill the gap
         -- there will be no break in the initialized portion of the
         heap.  Also, this is the last chunk allocated in the nursery, so it is
         safe to use the frontier from this processor as the global frontier.  */
        s->sharedHeap->oldGenSize = s->procStates[proc].sharedFrontier - s->sharedHeap->start;
      }
    }

    size_t maxBytes = s->lastSharedMajorStatistics->bytesLive + bytesRequested;

    /* This is the maximum size (over approximation) of the shared heap. We
     * will resize the heap after collection to a reasonable size. */
    for (int proc=0; proc < s->numberOfProcs; proc++) {
      GC_state r = &s->procStates[proc];
      objptr liftOp = r->forwardState.liftingObject;
      if (liftOp != BOGUS_OBJPTR)
        maxBytes += estimateSizeForLifting (r, objptrToPointer (liftOp, r->heap->start));
    }

    /* We are being optimistic with desired size since maxBytes is an
     * over-approzimation of live size */
    size_t desiredSize =
      ((s->controls->fixedHeap != 0 and maxBytes > s->controls->fixedHeap) or
       (s->controls->maxHeapShared != 0 and maxBytes > s->controls->maxHeapShared))
      ? maxBytes : sizeofHeapDesired (s, maxBytes, 0, SHARED_HEAP);
    if (DEBUG)
      fprintf (stderr, "performSharedGC: desiredSize=%s maxBytes=%s\n",
               uintmaxToCommaString (desiredSize),
               uintmaxToCommaString (maxBytes));
    if (desiredSize > s->secondarySharedHeap->size)
      resizeSharedHeapSecondary (s, desiredSize);
    if (not FORCE_MARK_COMPACT
        and (s->secondarySharedHeap->size != 0
             or createSharedHeapSecondary (s, desiredSize)))
      majorCheneyCopySharedGC (s);
    else {
      size_t newSize = 0;
      if (s->controls->fixedHeap != 0 and desiredSize > s->controls->fixedHeap)
        newSize = s->controls->fixedHeap;
      else if (s->controls->maxHeapShared != 0 and desiredSize > s->controls->maxHeapShared)
        newSize = s->controls->maxHeapShared;
      else if (desiredSize > s->sharedHeap->size)
        newSize = desiredSize;
      if (newSize > 0) {
        assert (newSize >= s->sharedHeap->oldGenSize);
        resizeHeap (s, s->sharedHeap, newSize);
      }
      majorMarkCompactSharedGC (s);
    }

    s->lastSharedMajorStatistics->bytesLive = s->sharedHeap->oldGenSize;
    if (s->lastSharedMajorStatistics->bytesLive > s->cumulativeStatistics->maxSharedBytesLive)
      s->cumulativeStatistics->maxSharedBytesLive = s->lastSharedMajorStatistics->bytesLive;
    resizeHeap (s, s->sharedHeap, s->lastSharedMajorStatistics->bytesLive + bytesRequested);
    resizeSharedHeapSecondary (s, s->sharedHeap->size);
    assert (s->sharedHeap->oldGenSize + bytesRequested <= s->sharedHeap->size);
    setGCStateCurrentSharedHeap (s, 0, 0, FALSE);
    s->cumulativeStatistics->bytesFilled += bytesFilled;
    if (DEBUG)
      fprintf (stderr, "[GC: Finished shared heap gc #%s; oldGenSize is %s]\n",
               uintmaxToCommaString(s->cumulativeStatistics->numCopyingSharedGCs +
                                    s->cumulativeStatistics->numMarkCompactSharedGCs),
               uintmaxToCommaString (s->sharedHeap->oldGenSize));
  }

  LEAVE0 (s);
  leaveGC (s);

  if (s->controls->reclaimObjects) {
    computeExclusivityInformation (s);
  }

  if (DEBUG)
    fprintf(stderr, "After performSharedGC: numDanglingStacks=%d [%d]\n",
            s->danglingStackListSize, s->procId);
}

void performGC (GC_state s,
                size_t oldGenBytesRequested,
                size_t nurseryBytesRequested,
                bool forceMajor,
                bool mayResize,
                size_t forceStackGrowthBytes) {
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
  /* Invariant does not hold if shared GCs are performed and read barriers */
  //assert (invariantForGC (s));
  if (needGCTime (s))
    startWallTiming (&tv_start);
  minorCheneyCopyGC (s);

  if (forceStackGrowthBytes > 0) {
    stackTopOk = FALSE;
    stackBytesRequested = forceStackGrowthBytes;
  }
  else {
    stackTopOk = invariantForMutatorStack (s);
    stackBytesRequested =
      stackTopOk
      ? 0
      : sizeofStackWithHeader (s, sizeofStackGrowReserved (s, getStackCurrent (s)));
  }

  totalBytesRequested =
    oldGenBytesRequested
    + stackBytesRequested;

  getThreadCurrent(s)->bytesNeeded = nurseryBytesRequested;

  if (nurseryBytesRequested == 0)
      nurseryBytesRequested = GC_HEAP_LIMIT_SLOP;
  totalBytesRequested += nurseryBytesRequested;

  if (forceMajor
      or totalBytesRequested > s->heap->availableSize - s->heap->oldGenSize)
    majorGC (s, totalBytesRequested, mayResize, TRUE);
  setGCStateCurrentLocalHeap (s, oldGenBytesRequested + stackBytesRequested,
                              nurseryBytesRequested);
  assert (hasHeapBytesFree (s, s->heap, oldGenBytesRequested + stackBytesRequested,
                            nurseryBytesRequested));
  unless (stackTopOk)
    growStackCurrent (s, TRUE, forceStackGrowthBytes);

  setGCStateCurrentThreadAndStack (s);
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
  assert (hasHeapBytesFree (s, s->heap, oldGenBytesRequested, nurseryBytesRequested));
  assert (invariantForGC (s));
  leaveGC (s);
}

size_t fillGap (__attribute__ ((unused)) GC_state s, pointer start, pointer end) {
  size_t diff = end - start;

  if (diff == 0) {
    return 0;
  }

  if (DEBUG_DETAILED)
    fprintf (stderr, "[GC: Filling gap between "FMTPTR" and "FMTPTR" (size = %zu).]\n",
             (uintptr_t)start, (uintptr_t)end, diff);

  if (start) {
    /* See note in the array case of foreach.c (line 103) */
    if (diff >= GC_ARRAY_HEADER_SIZE + OBJPTR_SIZE) {
      if (DEBUG_DETAILED)
          fprintf (stderr, "[GC: Filling gap with GC_ARRAY]\n");
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

#if ASSERT
    while (start < end) {
      *(start++) = 0xDF;
    }
#endif

    return diff;
  }
  else {
    return 0;
  }
}

/* returns TRUE if a shared heap GC was performed */
static bool allocChunkInSharedHeap (GC_state s,
                                    size_t bytesRequested) {

  s->cumulativeStatistics->bytesLifted += bytesRequested;
  while (TRUE)
  {
    /* This is the only read of the global frontier -- never read it again
       until after the swap. */
    pointer oldFrontier = s->sharedHeap->frontier;
    pointer newHeapFrontier, newProcFrontier;
    pointer newStart;
    /* heap->start and heap->size are read-only (unless you hold the global
       lock) so it's ok to read them here */
    size_t availableBytes = (size_t)((s->sharedHeap->start + s->sharedHeap->availableSize)
                                     - oldFrontier);

    assert (s->sharedLimitPlusSlop >= s->sharedFrontier);

    /* See if the mutator frontier invariant is already true */
    if (bytesRequested <= (size_t)(s->sharedLimitPlusSlop - s->sharedFrontier)) {
      if (DEBUG_DETAILED)
        fprintf (stderr, "[GC: aborting shared alloc: satisfied.] [%d]\n", s->procId);
      return FALSE;
    }

    /* alloc a chunk so that subsequent requests can be satisfied locally */
    if (bytesRequested < s->controls->allocChunkSize)
        bytesRequested = s->controls->allocChunkSize;

    /* Perhaps there is not enough space in the nursery to satify this
       request; if that's true then we need to do a full collection */
    if (bytesRequested + GC_BONUS_SLOP > availableBytes) {
      performSharedGC (s, bytesRequested + GC_BONUS_SLOP);
      return TRUE;
    }

    /* OK! We might possibly satisfy this request without the runtime lock!
       Let's see what that will entail... */
    /* Now see if we were the most recent thread to allocate */
    if (oldFrontier == s->sharedLimitPlusSlop + GC_BONUS_SLOP) {
      /* This is the next chunk so no need to fill */
      newHeapFrontier = s->sharedFrontier + bytesRequested + GC_BONUS_SLOP;
      /* Leave "start" and "frontier" where they are */
      newStart = s->sharedStart;
      newProcFrontier = s->sharedFrontier;
    }
    else {
      /* Fill the old gap */
      fillGap (s, s->sharedFrontier, s->sharedLimitPlusSlop + GC_BONUS_SLOP);
      /* Don't update frontier or limitPlusSlop since we will either
         overwrite them (if we succeed) or just fill the same gap again
         (if we fail).  (There is no obvious other pair of values that
         we can set them to that is safe.) */
      newHeapFrontier = oldFrontier + bytesRequested + GC_BONUS_SLOP;
      newProcFrontier = oldFrontier;
      /* Move "start" since the space between old-start and frontier is not
         necessary filled */
      newStart = oldFrontier;
    }

    if (__sync_bool_compare_and_swap (&s->sharedHeap->frontier,
                                      oldFrontier, newHeapFrontier)) {
      if (DEBUG)
        fprintf (stderr, "[GC: Shared alloction of chunk @ "FMTPTR".] [%d]\n",
                 (uintptr_t)newProcFrontier, s->procId);

      s->sharedStart = newStart;
      s->sharedFrontier = newProcFrontier;
      assert (isFrontierAligned (s, s->sharedFrontier));
      s->sharedLimitPlusSlop = newHeapFrontier - GC_BONUS_SLOP;
      s->sharedLimit = s->sharedLimitPlusSlop - GC_HEAP_LIMIT_SLOP;

      return FALSE;
    }
    else {
      if (DEBUG)
        fprintf (stderr, "[GC: Contention for shared alloction (frontier is "FMTPTR").] [%d]\n",
                 (uintptr_t)s->sharedHeap->frontier, s->procId);
    }
  }
  return FALSE;
}


static void maybeSatisfyAllocationRequestLocally (GC_state s,
                                                  size_t nurseryBytesRequested) {
  /* First try and take another chunk from the shared nursery */
  while (TRUE)
  {
    /* This is the only read of the global frontier -- never read it again
       until after the swap. */
    pointer oldFrontier = s->heap->frontier;
    pointer newHeapFrontier, newProcFrontier;
    pointer newStart;
    /* heap->start and heap->size are read-only (unless you hold the global
       lock) so it's ok to read them here */
    size_t availableBytes = (size_t)((s->heap->start + s->heap->availableSize)
                                     - oldFrontier);

    /* See if the mutator frontier invariant is already true */
    assert (s->limitPlusSlop >= s->frontier);
    if (nurseryBytesRequested <= (size_t)(s->limitPlusSlop - s->frontier)) {
      if (DEBUG_DETAILED)
        fprintf (stderr, "[GC: aborting local alloc: satisfied.]\n");
      return;
    }
    /* Perhaps there is not enough space in the nursery to satify this
       request; if that's true then we need to do a full collection */
    if (nurseryBytesRequested + GC_BONUS_SLOP > availableBytes) {
      if (DEBUG)
        fprintf (stderr, "[GC: aborting local alloc: no space.]\n");
      return;
    }

    /* OK! We might possibly satisfy this request without the runtime lock!
       Let's see what that will entail... */

    /* Now see if we were the most recent thread to allocate */
    if (oldFrontier == s->limitPlusSlop + GC_BONUS_SLOP) {
      /* This is the next chunk so no need to fill */
      newHeapFrontier = s->frontier + nurseryBytesRequested + GC_BONUS_SLOP;
      /* Leave "start" and "frontier" where they are */
      newStart = s->start;
      newProcFrontier = s->frontier;
    }
    else {
      /* Fill the old gap */
      fillGap (s, s->frontier, s->limitPlusSlop + GC_BONUS_SLOP);
      /* Don't update frontier or limitPlusSlop since we will either
         overwrite them (if we succeed) or just fill the same gap again
         (if we fail).  (There is no obvious other pair of values that
         we can set them to that is safe.) */
      newHeapFrontier = oldFrontier + nurseryBytesRequested + GC_BONUS_SLOP;
      newProcFrontier = oldFrontier;
      /* Move "start" since the space between old-start and frontier is not
         necessary filled */
      newStart = oldFrontier;
    }

    if (__sync_bool_compare_and_swap (&s->heap->frontier,
                                      oldFrontier, newHeapFrontier)) {
      if (DEBUG)
        fprintf (stderr, "[GC: Local alloction of chunk @ "FMTPTR".]\n",
                 (uintptr_t)newProcFrontier);

      s->start = newStart;
      s->frontier = newProcFrontier;
      assert (isFrontierAligned (s, s->frontier));
      s->limitPlusSlop = newHeapFrontier - GC_BONUS_SLOP;
      s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;

      return;
    }
    else {
      if (DEBUG)
        fprintf (stderr, "[GC: Contention for alloction (frontier is "FMTPTR").]\n",
                 (uintptr_t)s->heap->frontier);
    }
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
                                                     size_t forceStackGrowthBytes) {
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
  if (forceStackGrowthBytes > 0) {
    stackTopOk = FALSE;
    stackBytesRequested = forceStackGrowthBytes;
  }
  else {
    stackTopOk = ((not ensureStack) or invariantForMutatorStack (s));
    stackBytesRequested =
      stackTopOk
      ? 0
      : sizeofStackWithHeader (s, sizeofStackGrowReserved (s, getStackCurrent (s)));
  }

  if (forceStackGrowthBytes && DEBUG_SPLICE)
      fprintf (stderr, "stackBytesRequested = %ld\n", stackBytesRequested);

  /* try to satisfy (at least part of the) request locally */
  maybeSatisfyAllocationRequestLocally (s, nurseryBytesRequested + stackBytesRequested);

  if (not stackTopOk
      and (hasHeapBytesFree (s, s->heap, 0, stackBytesRequested))) {
    if (DEBUG or s->controls->messages)
      fprintf (stderr, "GC: growing stack locally... [%d]\n",
               s->procStates ? Proc_processorNumber (s) : -1);
    growStackCurrent (s, FALSE, forceStackGrowthBytes);
    setGCStateCurrentThreadAndStack (s);
  }

  if (DEBUG or s->controls->messages) {
    fprintf (stderr, "GC: stackInvariant: %d,%d hasLocalHeapBytesFree: %d inSection: %d force: %d [%d]\n",
             ensureStack, ensureStack and invariantForMutatorStack (s),
             hasHeapBytesFree (s, s->heap, oldGenBytesRequested, nurseryBytesRequested),
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
      or (not hasHeapBytesFree (s, s->heap, oldGenBytesRequested, nurseryBytesRequested)
            and (s->syncReason = SYNC_HEAP))
      /* we are forcing a major collection */
      or (forceGC
           and (s->syncReason = SYNC_FORCE))
      ) {

    /* Copy the value here so other threads will see it (if we synchronize and
       one of the other threads does the work). */
    if (isObjptr (getThreadCurrentObjptr(s)))
      getThreadCurrent(s)->bytesNeeded = nurseryBytesRequested;

    ENTER_LOCAL0 (s);
    if (fromGCCollect and (not s->signalsInfo.amInSignalHandler))
        switchToSignalHandlerThreadIfNonAtomicAndSignalPending (s);
    if ((ensureStack and not invariantForMutatorStack (s))
        or not hasHeapBytesFree (s, s->heap, oldGenBytesRequested, nurseryBytesRequested)
        or forceGC) {
      performGC (s, oldGenBytesRequested, nurseryBytesRequested, forceGC, TRUE, forceStackGrowthBytes);
    }
    else
      if (DEBUG or s->controls->messages)
        fprintf (stderr, "GC: Skipping GC (inside of sync). [%d]\n", s->procStates ? Proc_processorNumber (s) : -1);
    LEAVE_LOCAL0 (s);
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
      assert (hasHeapBytesFree (s, s->heap, 0, 0));
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

  /* When the mutator requests zero bytes, it may actually need as
   * much as GC_HEAP_LIMIT_SLOP.
   */
  if (0 == bytesRequested)
    bytesRequested = s->controls->allocChunkSize;
  else if (bytesRequested < s->controls->allocChunkSize)
    bytesRequested = s->controls->allocChunkSize;
  else
    bytesRequested += GC_HEAP_LIMIT_SLOP;

  /* XXX copied from enter() */
  /* used needs to be set because the mutator has changed s->stackTop. */
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  getThreadCurrent(s)->bytesNeeded = bytesRequested;


  ensureHasHeapBytesFreeAndOrInvariantForMutator (s, force,
                                                  TRUE, TRUE,
                                                  0, 0, TRUE, 0);
  Parallel_maybeWaitForGC ();
}

pointer FFI_getOpArgsResPtr (GC_state s) {
  return s->ffiOpArgsResPtr;
}
