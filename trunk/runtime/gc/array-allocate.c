/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

pointer GC_arrayAllocate (GC_state s,
                          size_t ensureBytesFree,
                          GC_arrayLength numElements,
                          GC_header header) {
  uintmax_t arraySizeMax, arraySizeAlignedMax;
  size_t arraySize, arraySizeAligned;
  size_t bytesPerElement;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  pointer frontier;
  pointer last;
  pointer result;
  bool holdLock;

  splitHeader(s, header, NULL, NULL, NULL, &bytesNonObjptrs,
              &numObjptrs, NULL, NULL);
  if (DEBUG_ARRAY)
    fprintf (stderr, "GC_arrayAllocate (%"PRIuMAX", "FMTARRLEN", "FMTHDR") [%d]\n",
             (uintmax_t)ensureBytesFree, numElements, header, Proc_processorNumber (s));
  bytesPerElement = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
  arraySizeMax =
    (uintmax_t)bytesPerElement * (uintmax_t)numElements + GC_ARRAY_HEADER_SIZE;
  arraySizeAlignedMax = alignMax (arraySizeMax, s->alignment);
  if (arraySizeAlignedMax >= (uintmax_t)SIZE_MAX)
    die ("Out of memory.  Unable to allocate array with %s bytes.",
         uintmaxToCommaString(arraySizeAlignedMax));
  arraySize = (size_t)arraySizeMax;
  arraySizeAligned = (size_t)arraySizeAlignedMax;
  if (arraySizeAligned < GC_ARRAY_HEADER_SIZE + OBJPTR_SIZE) {
    /* Very small (including empty) arrays have OBJPTR_SIZE bytes
     * space for the forwarding pointer.
     */
    arraySize = GC_ARRAY_HEADER_SIZE;
    arraySizeAligned = align(GC_ARRAY_HEADER_SIZE + OBJPTR_SIZE, s->alignment);
  }
  if (DEBUG_ARRAY)
    fprintf (stderr,
             "Array with "FMTARRLEN" elts of size %"PRIuMAX" and total size %s and total aligned size %s.  "
             "Ensure %s bytes free.\n",
             numElements, (uintmax_t)bytesPerElement,
             uintmaxToCommaString(arraySize),
             uintmaxToCommaString(arraySizeAligned),
             uintmaxToCommaString(ensureBytesFree));

  /* Determine whether we will perform this allocation locally or not */
  holdLock = arraySizeAligned >= s->controls->oldGenArraySize;

  if (holdLock) {
    s->syncReason = SYNC_OLD_GEN_ARRAY;
    ENTER_LOCAL0 (s);
    if (not hasHeapBytesFree (s, s->heap, arraySizeAligned, ensureBytesFree)) {
      performGC (s, arraySizeAligned, ensureBytesFree, FALSE, TRUE, 0);
    }
    assert (hasHeapBytesFree (s, s->heap, arraySizeAligned, ensureBytesFree));
    frontier = s->heap->start + s->heap->oldGenSize;
    assert (isFrontierAligned (s, frontier));

    /* This must be updated while holding the lock! */
    s->heap->oldGenSize += arraySizeAligned;
    assert (s->heap->start + s->heap->oldGenSize <= s->heap->nursery);
    s->cumulativeStatistics->bytesAllocated += arraySizeAligned;
    /* NB LEAVE appears below since no heap invariant holds while the
       oldGenSize has been updated but the array remains uninitialized. */
  } else {
    /* Local alloc */
    size_t bytesRequested;
    pointer newFrontier;

    bytesRequested = arraySizeAligned + ensureBytesFree;
    if (not hasHeapBytesFree (s, s->heap, 0, bytesRequested)) {
      /* Local alloc may still require getting the lock, but we will release
         it before initialization. */
      ensureHasHeapBytesFreeAndOrInvariantForMutator (s, FALSE, FALSE, FALSE,
                                                      0, bytesRequested, FALSE, 0);
    }
    assert (hasHeapBytesFree (s, s->heap, 0, bytesRequested));
    frontier = s->frontier;
    newFrontier = frontier + arraySizeAligned;
    assert (isFrontierAligned (s, newFrontier));
    s->frontier = newFrontier;
  }

  /* Now do the initialization */
  last = frontier + arraySize;
  *((GC_arrayCounter*)(frontier)) = 0;
  frontier = frontier + GC_ARRAY_COUNTER_SIZE;
  *((GC_arrayLength*)(frontier)) = numElements;
  frontier = frontier + GC_ARRAY_LENGTH_SIZE;
  *((GC_header*)(frontier)) = header;
  frontier = frontier + GC_HEADER_SIZE;
  result = frontier;
  assert (isAligned ((size_t)result, s->alignment));
  /* Initialize all pointers with BOGUS_OBJPTR. */
  if (1 <= numObjptrs and 0 < numElements) {
    pointer p;

    if (0 == bytesNonObjptrs)
      for (p = frontier; p < last; p += OBJPTR_SIZE)
        *((objptr*)p) = BOGUS_OBJPTR;
    else {
      /* Array with a mix of pointers and non-pointers. */
      size_t bytesObjptrs;

      bytesObjptrs = numObjptrs * OBJPTR_SIZE;

      for (p = frontier; p < last; ) {
        pointer next;

        p += bytesNonObjptrs;
        next = p + bytesObjptrs;
        assert (next <= last);
        for ( ; p < next; p += OBJPTR_SIZE)
          *((objptr*)p) = BOGUS_OBJPTR;
      }
    }
  }
  GC_profileAllocInc (s, arraySizeAligned);
  if (DEBUG_ARRAY) {
    fprintf (stderr, "GC_arrayAllocate done.  result = "FMTPTR"  frontier = "FMTPTR" [%d]\n",
             (uintptr_t)result, (uintptr_t)s->frontier, Proc_processorNumber (s));
    displayGCState (s, stderr);
  }
  assert (ensureBytesFree <= (size_t)(s->limitPlusSlop - s->frontier));
  /* Unfortunately, the invariant isn't quite true here, because
   * unless we did the GC, we never set s->currentThread->stack->used
   * to reflect what the mutator did with stackTop.
   */

  if (holdLock) {
    LEAVE_LOCAL1 (s, result);
  }

  return result;
}
