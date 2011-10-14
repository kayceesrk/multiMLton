/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                 Jonkers Mark-compact Collection                  */
/* ---------------------------------------------------------------- */

void copyForThreadInternal (pointer dst, pointer src) {
  if (FALSE)
    fprintf (stderr,
             "copyForThreadInternal dst = "FMTPTR"  src = "FMTPTR"\n",
             (uintptr_t)dst, (uintptr_t)src);
  if (OBJPTR_SIZE > GC_HEADER_SIZE) {
    size_t count;

    assert (0 == (OBJPTR_SIZE % GC_HEADER_SIZE));
    count = (OBJPTR_SIZE - GC_HEADER_SIZE) / GC_HEADER_SIZE;
    src = src + GC_HEADER_SIZE * count;

    for (size_t i = 0; i <= count; i++) {
      *((GC_header*)dst) = *((GC_header*)src);
      dst += GC_HEADER_SIZE;
      src -= GC_HEADER_SIZE;
    }
  } else if (GC_HEADER_SIZE > OBJPTR_SIZE) {
    size_t count;

    assert (0 == (GC_HEADER_SIZE % OBJPTR_SIZE));
    count = (GC_HEADER_SIZE - OBJPTR_SIZE) / OBJPTR_SIZE;
    dst = dst + OBJPTR_SIZE * count;

    for (size_t i = 0; i <= count; i++) {
      *((objptr*)dst) = *((objptr*)src);
      dst -= OBJPTR_SIZE;
      src += OBJPTR_SIZE;
    }
  } else /* (GC_HEADER_SIZE == OBJPTR_SIZE) */ {
    *((GC_header*)dst) = *((GC_header*)src);
  }
}

void threadInternalObjptr (GC_state s, objptr *opp) {
  objptr opop;
  pointer p;
  GC_header *headerp;

  opop = pointerToObjptr ((pointer)opp, s->heap->start);
  p = objptrToPointer (*opp, s->heap->start);
  if (DEBUG_MARK_COMPACT)
    fprintf (stderr,
             "threadInternal opp = "FMTPTR"  p = "FMTPTR"  header = "FMTHDR"\n",
             (uintptr_t)opp, (uintptr_t)p, getHeader (p));
  headerp = getHeaderp (p);
  copyForThreadInternal ((pointer)(opp), (pointer)(headerp));
  copyForThreadInternal ((pointer)(headerp), (pointer)(&opop));
}

void threadInternalObjptrIfInLocalHeap (GC_state s, objptr *opp) {
  fixFwdObjptr (s, opp);
  pointer p = objptrToPointer (*opp, s->heap->start);
  if (isPointerInHeap (s, s->heap, p))
    threadInternalObjptr (s, opp);
  else {
    if (DEBUG_MARK_COMPACT)
      fprintf (stderr, "threadInternalObjptrIfInLocalHeap skipped. opp="FMTPTR" p="FMTPTR"\n",
             (uintptr_t)opp, (uintptr_t)p);
  }
}

void threadInternalObjptrIfInSharedHeap (GC_state s, objptr *opp) {
  fixFwdObjptr (s, opp);
  pointer p = objptrToPointer (*opp, s->sharedHeap->start);
  if (isPointerInHeap (s, s->sharedHeap, p))
    threadInternalObjptr (s, opp);
  else {
    if (DEBUG_MARK_COMPACT)
      fprintf (stderr, "threadInternalObjptrIfInSharedHeap skipped. opp="FMTPTR" p="FMTPTR"\n",
             (uintptr_t)opp, (uintptr_t)p);
  }
}

/* If the object pointer is valid, and points to an unmarked object,
 * then clear the object pointer.
 */
void updateWeaksForMarkCompact (GC_state s) {
  pointer p;
  GC_weak w;

  for (w = s->weaks; w != NULL; w = w->link) {
    assert (BOGUS_OBJPTR != w->objptr);

    if (DEBUG_WEAK)
      fprintf (stderr, "updateWeaksForMarkCompact  w = "FMTPTR"  ", (uintptr_t)w);

    if (BOGUS_OBJPTR == w->objptr) {
      if (DEBUG_WEAK)
        fprintf (stderr, "already cleared\n");
      continue;
    }

    p = objptrToPointer(w->objptr, s->heap->start);
    /* If it's unmarked, clear the weak pointer. */
    if (isPointerMarked(p)) {
      if (DEBUG_WEAK)
        fprintf (stderr, "not cleared\n");
    } else {
      if (DEBUG_WEAK)
        fprintf (stderr, "cleared\n");
      *(getHeaderp((pointer)w - offsetofWeak (s))) = GC_WEAK_GONE_HEADER | MARK_MASK;
      w->objptr = BOGUS_OBJPTR;
    }
  }
  s->weaks = NULL;
}

void updateForwardPointersForMarkCompact (GC_state s,
                                          GC_heap h,
                                          pointer front,
                                          pointer back,
                                          GC_stack* currentStack,
                                          bool sharedCollection) {
  pointer endOfLastMarked;
  size_t gap;
  GC_header header;
  GC_header *headerp;
  pointer p;
  size_t size, skipFront, skipGap;

  if (DEBUG_MARK_COMPACT)
    fprintf (stderr, "Update forward pointers.\n");
  gap = 0;
  endOfLastMarked = front;
updateObject:
  if (DEBUG_MARK_COMPACT)
    fprintf (stderr, "updateObject  front = "FMTPTR"  back = "FMTPTR"\n",
             (uintptr_t)front, (uintptr_t)back);
  if (front == back)
    goto done;
  p = advanceToObjectData (s, front);
  headerp = getHeaderp (p);
  header = *headerp;
  if (GC_VALID_HEADER_MASK & header) {
    /* It's a header */
    if (MARK_MASK & header) {
      /* It is marked, but has no forward pointers.
       * Thread internal pointers.
       */
thread:
      assert (GC_VALID_HEADER_MASK & header);
      assert (MARK_MASK & header);

      size_t headerBytes, objectBytes;
      GC_objectTypeTag tag;
      uint16_t bytesNonObjptrs, numObjptrs;

      assert (header == getHeader (p));

      if (header == GC_FORWARDED) {
        /* forwarded object. Object resides in the shared heap. Hence skip */
        size = sizeofObject (s, p);
        /* If we are performing sharedCollection and walking local heaps, don't
         * consider unmarked objects as dead. Hence, only increment gap
         * (signifying death), if we are performing local collection or walking
         * the shared heap during shared collection. */
        if ((not sharedCollection) || (h==s->sharedHeap)) {
          gap += size;
        }
        else {
          /* We need to fill the forwarded object in the local heap during
           * shared collection so that subsequent mark-compactions can skip
           * this object. At this point, there will be no pointers to this
           * objects since forwarding pointers have been fixed before entering
           * shared GC. */
          fillGap (s, front, front + size);
        }
        front += size;
        if (DEBUG_MARK_COMPACT) {
          fprintf (stderr, "updateForwardPointers: threading saw forwarded object.\n");
          fprintf (stderr, "\tp="FMTPTR" of size=%ld. New gap=%ld. New front="FMTPTR"\n",
                   (uintptr_t)p, size, gap, (uintptr_t)front);
        }
        goto updateObject;
      }

      splitHeader(s, header, getHeaderp (p), &tag, NULL,
                  &bytesNonObjptrs, &numObjptrs, NULL, NULL);

      /* Compute the space taken by the header and object body. */
      if ((NORMAL_TAG == tag) or (WEAK_TAG == tag)) { /* Fixed size object. */
        headerBytes = GC_NORMAL_HEADER_SIZE;
        objectBytes = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
        skipFront = 0;
        skipGap = 0;
      } else if (ARRAY_TAG == tag) {
        headerBytes = GC_ARRAY_HEADER_SIZE;
        objectBytes = sizeofArrayNoHeader (s, getArrayLength (p),
                                           bytesNonObjptrs, numObjptrs);
        skipFront = 0;
        skipGap = 0;
      } else { /* Stack. */
        bool current;
        size_t reservedNew, reservedOld;
        GC_stack stack;

        assert (STACK_TAG == tag);
        headerBytes = GC_STACK_HEADER_SIZE;
        stack = (GC_stack)p;
        if (not sharedCollection)
          current = (*currentStack) == stack;
        else {
          current = FALSE;
          for (int proc = 0; proc < s->numberOfProcs; proc++)
            current = (current || (currentStack[proc] == stack));
        }


        if (sharedCollection and //We are performing this for a shared collection
            (not (h == s->sharedHeap))) {//Current heap is not the shared heap
          skipGap = 0;
          skipFront = 0;
          objectBytes = sizeof (struct GC_stack) + stack->reserved;
        }
        else {
          reservedOld = stack->reserved;
          reservedNew = sizeofStackShrinkReserved (s, stack, current);
          objectBytes = sizeof (struct GC_stack) + stack->used;
          skipFront = reservedOld - stack->used;
          skipGap = reservedOld - reservedNew;
        }
      }
      size = headerBytes + objectBytes;
      if (DEBUG_MARK_COMPACT)
        fprintf (stderr, "threading "FMTPTR" of size %"PRIuMAX"\n",
                 (uintptr_t)p, (uintmax_t)size);
      if (((size_t)(front - endOfLastMarked) >= GC_ARRAY_HEADER_SIZE + OBJPTR_SIZE) and
          //Dont compress for localHeaps during shared collection
          (not sharedCollection || h==s->sharedHeap)) {
        pointer newArray = endOfLastMarked;
        /* Compress all of the unmarked into one vector.  We require
         * (GC_ARRAY_HEADER_SIZE + OBJPTR_SIZE) space to be available
         * because that is the smallest possible array.  You cannot
         * use GC_ARRAY_HEADER_SIZE because even very small (including
         * zero-length) arrays require extra space for the forwarding
         * pointer.  If you did use GC_ARRAY_HEADER_SIZE,
         * updateBackwardPointersAndSlideForMarkCompact would skip the
         * extra space and be completely busted.
         */
        if (DEBUG_MARK_COMPACT)
          fprintf (stderr, "compressing from "FMTPTR" to "FMTPTR" (length = %"PRIuMAX")\n",
                   (uintptr_t)endOfLastMarked, (uintptr_t)front,
                   (uintmax_t)(front - endOfLastMarked));
        *((GC_arrayCounter*)(newArray)) = 0;
        newArray += GC_ARRAY_COUNTER_SIZE;
        *((GC_arrayLength*)(newArray)) =
          ((size_t)(front - endOfLastMarked)) - GC_ARRAY_HEADER_SIZE;
        newArray += GC_ARRAY_LENGTH_SIZE;
        *((GC_header*)(newArray)) = GC_WORD8_VECTOR_HEADER;
      }
      gap += skipGap;
      front += size + skipFront;
      endOfLastMarked = front;
      if (sharedCollection)
        foreachObjptrInObject (s, p, threadInternalObjptrIfInSharedHeap, FALSE);
      else
        foreachObjptrInObject (s, p, threadInternalObjptrIfInLocalHeap, FALSE);
      goto updateObject;
    } else {
      /* It's not marked. */
      size = sizeofObject (s, p);
      if (DEBUG_MARK_COMPACT)
        fprintf (stderr, "skipping "FMTPTR" of size %"PRIuMAX"\n",
                 (uintptr_t)p, (uintmax_t)size);
      /* If we are performing sharedCollection and walking local heaps, don't
        * consider unmarked objects as dead */
      if ((not sharedCollection) || (h==s->sharedHeap))
        gap += size;
      else
        fillGap (s, front, front+size);
      front += size;
      goto updateObject;
    }
  } else {
    /* We should ONLY get here if we are performing local collection or we are
     * performing shared collection and walking shared heap */
    assert ((not sharedCollection) || (h == s->sharedHeap));

    pointer new;
    objptr newObjptr;

    assert (not (GC_VALID_HEADER_MASK & header));
    /* It's a pointer.  This object must be live.  Fix all the forward
     * pointers to it, store its header, then thread its internal
     * pointers.
     */
    new = p - gap;
    newObjptr = pointerToObjptr (new, h->start);
    do {
      pointer cur;
      objptr curObjptr;

      copyForThreadInternal ((pointer)(&curObjptr), (pointer)headerp);
      cur = objptrToPointer (curObjptr, h->start);

      copyForThreadInternal ((pointer)headerp, cur);
      *((objptr*)cur) = newObjptr;

      header = *headerp;
    } while (0 == (1 & header));
    goto thread;
  }
  assert (FALSE);
done:
  #if ASSERT
  if (sharedCollection and (not (h == s->sharedHeap))) {
    assert (gap == 0);
  }
  #endif
  return;
}

void updateBackwardPointersAndSlideForMarkCompact (GC_state s, GC_heap h, GC_stack* currentStack, bool sharedCollection) {
  pointer back;
  pointer front;
  size_t gap;
  GC_header header;
  GC_header *headerp;
  pointer p;
  size_t size, skipFront, skipGap;

  if (DEBUG_MARK_COMPACT)
    fprintf (stderr, "Update backward pointers and slide.\n");
  front = alignFrontier (s, h->start);
  back = h->start + h->oldGenSize;
  gap = 0;
updateObject:
  if (DEBUG_MARK_COMPACT)
    fprintf (stderr, "updateObject  front = "FMTPTR"  back = "FMTPTR"\n",
             (uintptr_t)front, (uintptr_t)back);
  if (front == back)
    goto done;
  p = advanceToObjectData (s, front);
  headerp = getHeaderp (p);
  header = *headerp;
  if (GC_VALID_HEADER_MASK & header) {
    /* It's a header */
    if (MARK_MASK & header) {
      /* It is marked, but has no backward pointers to it.
       * Unmark it.
       */
unmark:
      assert (GC_VALID_HEADER_MASK & header);
      assert (MARK_MASK & header);

      size_t headerBytes, objectBytes;
      GC_objectTypeTag tag;
      uint16_t bytesNonObjptrs, numObjptrs;

      assert (header == getHeader (p));

      if (header == GC_FORWARDED) {
        /* forwarded object. Object resides in the shared heap. Hence skip */
        size = sizeofObject (s, p);
        gap += size;
        front += size;
        if (DEBUG_MARK_COMPACT) {
          fprintf (stderr, "updateBackwardPointers: threading saw forwarded object.\n");
          fprintf (stderr, "\tp="FMTPTR" of size=%ld. New gap=%ld. New front="FMTPTR"\n",
                   (uintptr_t)p, size, gap, (uintptr_t)front);
        }
        goto updateObject;
      }
      splitHeader(s, header, getHeaderp (p), &tag, NULL,
                  &bytesNonObjptrs, &numObjptrs, NULL, NULL);

      /* Compute the space taken by the header and object body. */
      if ((NORMAL_TAG == tag) or (WEAK_TAG == tag)) { /* Fixed size object. */
        headerBytes = GC_NORMAL_HEADER_SIZE;
        objectBytes = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
        skipFront = 0;
        skipGap = 0;
      } else if (ARRAY_TAG == tag) {
        headerBytes = GC_ARRAY_HEADER_SIZE;
        objectBytes = sizeofArrayNoHeader (s, getArrayLength (p),
                                           bytesNonObjptrs, numObjptrs);
        skipFront = 0;
        skipGap = 0;
      } else { /* Stack. */
        bool current;
        size_t reservedNew, reservedOld;
        GC_stack stack;

        assert (STACK_TAG == tag);
        headerBytes = GC_STACK_HEADER_SIZE;
        stack = (GC_stack)p;
        if (not sharedCollection)
          current = (*currentStack) == stack;
        else {
          current = FALSE;
          for (int proc = 0; proc < s->numberOfProcs; proc++)
            current = (current || (currentStack[proc] == stack));
        }

        reservedOld = stack->reserved;
        reservedNew = sizeofStackShrinkReserved (s, stack, current);
        if (reservedNew < stack->reserved) {
          if (DEBUG_STACKS or s->controls->messages)
            fprintf (stderr,
                     "[GC: Shrinking stack of size %s bytes to size %s bytes, using %s bytes.]\n",
                     uintmaxToCommaString(stack->reserved),
                     uintmaxToCommaString(reservedNew),
                     uintmaxToCommaString(stack->used));
          stack->reserved = reservedNew;
        }
        objectBytes = sizeof (struct GC_stack) + stack->used;
        skipFront = reservedOld - stack->used;
        skipGap = reservedOld - reservedNew;

        pointer thrd = objptrToPointer (stack->thread, h->start);
        if (getHeader(thrd) == GC_FORWARDED) {
          stack->thread = *(objptr*)thrd;
        }

        if (DEBUG_DETAILED)
          fprintf (stderr, "[GC: Sliding stack. stack->thread is "FMTOBJPTR"]\n", stack->thread);
      }
      size = headerBytes + objectBytes;
      /* unmark */
      if (DEBUG_MARK_COMPACT)
        fprintf (stderr, "unmarking "FMTPTR" of size %"PRIuMAX"\n",
                 (uintptr_t)p, (uintmax_t)size);
      *headerp = header & ~MARK_MASK;
      /* slide */
      if (DEBUG_MARK_COMPACT)
        fprintf (stderr, "sliding "FMTPTR" down %"PRIuMAX"\n",
                 (uintptr_t)front, (uintmax_t)gap);
      GC_memcpy (front, front - gap, size);
      gap += skipGap;
      front += size + skipFront;
      goto updateObject;
    } else {
      /* It's not marked. */
      size = sizeofObject (s, p);
      if (DEBUG_MARK_COMPACT)
        fprintf (stderr, "skipping "FMTPTR" of size %"PRIuMAX"\n",
                 (uintptr_t)p, (uintmax_t)size);
      gap += size;
      front += size;
      goto updateObject;
    }
  } else {
    pointer new;
    objptr newObjptr;

    assert (not (GC_VALID_HEADER_MASK & header));
    /* It's a pointer.  This object must be live.  Fix all the
     * backward pointers to it.  Then unmark it.
     */
    new = p - gap;
    newObjptr = pointerToObjptr (new, h->start);
    do {
      pointer cur;
      objptr curObjptr;

      copyForThreadInternal ((pointer)(&curObjptr), (pointer)headerp);
      cur = objptrToPointer (curObjptr, h->start);

      copyForThreadInternal ((pointer)headerp, cur);
      *((objptr*)cur) = newObjptr;

      header = *headerp;
    } while (0 == (1 & header));
    /* The unmarked header will be stored by unmark. */
    goto unmark;
  }
  assert (FALSE);
done:
  h->oldGenSize = front - gap - h->start;
  if (DEBUG_MARK_COMPACT)
    fprintf (stderr, "oldGenSize = %"PRIuMAX"\n",
             (uintmax_t)h->oldGenSize);
  return;
}

void majorMarkCompactGC (GC_state s) {
  size_t bytesHashConsed;
  size_t bytesMarkCompacted;
  GC_stack currentStack;
  struct rusage ru_start;

  if (detailedGCTime (s))
    startTiming (&ru_start);
  s->cumulativeStatistics->numMarkCompactGCs++;
  if (DEBUG or s->controls->messages) {
    fprintf (stderr,
             "[GC: Starting major mark-compact;]\n");
    fprintf (stderr,
             "[GC:\theap at "FMTPTR" of size %s bytes.]\n",
             (uintptr_t)(s->heap->start),
             uintmaxToCommaString(s->heap->size));
  }

  //Mark compact GC core
  currentStack = getStackCurrent (s);
  if (s->hashConsDuringGC) {
    s->lastMajorStatistics->bytesHashConsed = 0;
    s->cumulativeStatistics->numHashConsGCs++;
    s->objectHashTable = allocHashTable (s);
    foreachGlobalObjptrInScope (s, dfsMarkWithHashConsWithLinkWeaks);
    freeHashTable (s->objectHashTable);
  } else {
    foreachGlobalObjptrInScope (s, dfsMarkWithoutHashConsWithLinkWeaks);
  }
  updateWeaksForMarkCompact (s);
  foreachGlobalObjptrInScope (s, threadInternalObjptrIfInLocalHeap);
  updateForwardPointersForMarkCompact (s, s->heap, alignFrontier(s, s->heap->start),
                                       s->heap->start + s->heap->oldGenSize, &currentStack, FALSE);
  updateBackwardPointersAndSlideForMarkCompact (s, s->heap, &currentStack, FALSE);

  //Collect statistics
  bytesHashConsed = s->lastMajorStatistics->bytesHashConsed;
  s->cumulativeStatistics->bytesHashConsed += bytesHashConsed;
  bytesMarkCompacted = s->heap->oldGenSize;
  s->cumulativeStatistics->bytesMarkCompacted += bytesMarkCompacted;
  s->lastMajorStatistics->kind = GC_MARK_COMPACT;
  if (detailedGCTime (s))
    stopTiming (&ru_start, &s->cumulativeStatistics->ru_gcMarkCompact);
  if (DEBUG or s->controls->messages) {
    fprintf (stderr,
             "[GC: Finished major mark-compact; mark compacted %s bytes.]\n",
             uintmaxToCommaString(bytesMarkCompacted));
    if (s->hashConsDuringGC)
      printBytesHashConsedMessage(bytesHashConsed,
                                  bytesHashConsed + bytesMarkCompacted);
  }
}

void headerCheckInternalObjptr (GC_state s, objptr* opp) {
  pointer p = objptrToPointer (*opp, s->heap->start);
  GC_header header = getHeader (p);
  assert (not (header & MARK_MASK) || header==GC_FORWARDED);
  assert (GC_VALID_HEADER_MASK & header);
  header = header;
}

void headerCheck (GC_state s, pointer front, pointer back) {
  while (front < back) {
    pointer p = advanceToObjectData (s, front);
    GC_header header = getHeader (p);
    assert (not (header & MARK_MASK) || header==GC_FORWARDED);
    assert (GC_VALID_HEADER_MASK & header);
    if (not (header==GC_FORWARDED))
      foreachObjptrInObject (s, p, headerCheckInternalObjptr, TRUE);
    front += sizeofObject (s, p);
    header = header;
  }
}

void majorMarkCompactSharedGC (GC_state s) {
  size_t bytesMarkCompacted;
  GC_stack* currentStacks;
  struct rusage ru_start;

  if (detailedGCTime (s))
    startTiming (&ru_start);
  s->cumulativeStatistics->numMarkCompactSharedGCs++;
  if (DEBUG or s->controls->messages) {
    fprintf (stderr,
             "[GC: Starting shared major mark-compact] [%d]\n", s->procId);
    fprintf (stderr,
             "[GC:\theap at "FMTPTR" of size %s bytes,] [%d]\n",
             (uintptr_t)(s->sharedHeap->start),
             uintmaxToCommaString(s->sharedHeap->size), s->procId);
  }

  //Algorithm Core
  currentStacks = (GC_stack*) malloc (sizeof (GC_stack) * s->numberOfProcs);
  for (int proc = 0; proc < s->numberOfProcs; proc++)
    currentStacks[proc] = getStackCurrent (&s->procStates[proc]);

  //Mark all live objects in all heaps
  /* NOTE: dangling stacks are cleared, hence, we will only trace dangling
   * stacks if they are either pointed to by global pointers (currentThread,
   * signalHandlerThread, etc.) or if the thread is pointed to by some other
   * live object */
  if (DEBUG_DETAILED)
    fprintf (stderr, "majorMarkCompactSharedGC(1)\n");
  foreachGlobalObjptr (s, dfsMarkTraceShared);

  if (DEBUG_DETAILED)
    fprintf (stderr, "majorMarkCompactSharedGC(2)\n");
  for (int proc=0; proc < s->numberOfProcs; proc++) {
    GC_state r = &s->procStates[proc];
    updateWeaksForMarkCompact (r);
  }

  if (DEBUG_DETAILED)
    fprintf (stderr, "majorMarkCompactSharedGC(3)\n");
  foreachGlobalObjptr (s, threadInternalObjptrIfInSharedHeap);

  if (DEBUG_DETAILED)
    fprintf (stderr, "majorMarkCompactSharedGC(4)\n");
  //Walk local heaps threading pointers to shared heap
  for (int proc=0; proc < s->numberOfProcs; proc++) {
    GC_state r = &s->procStates[proc];
    if (DEBUG_DETAILED)
      fprintf (stderr, "majorMarkCompactSharedGC(5) [%d]\n", r->procId);
    updateForwardPointersForMarkCompact (r, r->heap, alignFrontier (r, r->heap->start),
                                         r->heap->start + r->heap->oldGenSize,
                                         currentStacks, TRUE);
    if (r->frontier > r->heap->start + r->heap->oldGenSize)
      updateForwardPointersForMarkCompact (r, r->heap, alignFrontier (r, r->heap->nursery),
                                           r->frontier, currentStacks, TRUE);
  }

  if (DEBUG_DETAILED)
    fprintf (stderr, "majorMarkCompactSharedGC(6)\n");

  //Walk the shared heap threading pointers. At the end of this phase, all
  //pointer from local heaps should point to the new location on the shared
  //heap.
  updateForwardPointersForMarkCompact (s, s->sharedHeap, alignFrontier (s, s->sharedHeap->start),
                                       s->sharedHeap->start + s->sharedHeap->oldGenSize, currentStacks, TRUE);

  if (DEBUG_DETAILED)
    fprintf (stderr, "majorMarkCompactSharedGC(7)\n");
  updateBackwardPointersAndSlideForMarkCompact (s, s->sharedHeap, currentStacks, TRUE);

  s->forwardState.toStart = alignFrontier (s, s->sharedHeap->start);
  s->forwardState.toLimit = s->sharedHeap->start + s->sharedHeap->size;
  s->forwardState.back = s->sharedHeap->start + s->sharedHeap->oldGenSize;

  if (DEBUG_DETAILED)
    fprintf (stderr, "majorMarkCompactSharedGC(8)\n");
  /* Walk the shared heap recreating danglingStackList and lifting partially
   * lifted closures. This also unmarks the danglingStacks and lifted objects.
   * */
  foreachObjptrInRange (s, s->forwardState.toStart, &s->forwardState.back, forwardObjptrForSharedMarkCompact, TRUE);
  s->sharedHeap->oldGenSize = s->forwardState.back - s->forwardState.toStart;

  if (DEBUG_DETAILED)
    fprintf (stderr, "majorMarkCompactSharedGC(9)\n");
  //Now, unmark the live local heap objects
  foreachGlobalObjptr (s, dfsUnmark);

  //Collect statistics
  bytesMarkCompacted = s->sharedHeap->oldGenSize;
  s->cumulativeStatistics->bytesMarkCompactedShared += bytesMarkCompacted;
  s->lastSharedMajorStatistics->kind = GC_MARK_COMPACT;

  free (currentStacks);
  if (detailedGCTime (s))
    stopTiming (&ru_start, &s->cumulativeStatistics->ru_gcMarkCompactShared);

  if (DEBUG_DETAILED)
    fprintf (stderr, "majorMarkCompactSharedGC(10)\n");

  #if ASSERT
  for (int proc=0; proc < s->numberOfProcs; proc++) {
    GC_state r = &s->procStates[proc];
    if (DEBUG)
      fprintf (stderr, "Checking the headers of local heap (1) [%d]\n", r->procId);
    pointer front = r->heap->start;
    pointer back = r->heap->start + r->heap->oldGenSize;
    headerCheck (r, front, back);
    if (r->frontier > back) {
      front = r->heap->nursery;
      back = r->frontier;
      headerCheck (r, front, back);
    }
    if (DEBUG)
      fprintf (stderr, "Checking the headers of local heap (2) [%d]\n", r->procId);
  }
  if (DEBUG)
    fprintf (stderr, "Checking the headers of shared heap (1)\n");
  headerCheck (s, s->sharedHeap->start, s->sharedHeap->start + s->sharedHeap->oldGenSize);
  if (DEBUG)
    fprintf (stderr, "Checking the headers of shared heap (2)\n");
  #endif

  if (DEBUG or s->controls->messages)
    fprintf (stderr, "[GC: Finished shared major mark-compact.]\n");
}
