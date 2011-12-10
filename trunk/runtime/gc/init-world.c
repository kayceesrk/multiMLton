/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                          Initialization                          */
/* ---------------------------------------------------------------- */

size_t sizeofIntInfFromString (GC_state s, const char *str) {
  size_t slen = strlen (str);

  /* A slight overestimate. */
  double bytesPerChar = 0.415241011861 /* = ((log(10.0) / log(2.0)) / 8.0) */ ;
  double bytes = ceil((double)slen * bytesPerChar);
  return align (GC_ARRAY_HEADER_SIZE
                + sizeof(mp_limb_t) // for the sign
                + align((size_t)bytes, sizeof(mp_limb_t)),
                s->alignment);
}

size_t sizeofInitialBytesLive (GC_state s) {
  uint32_t i;
  size_t dataBytes;
  size_t total;

  total = 0;
  for (i = 0; i < s->intInfInitsLength; ++i) {
    total += sizeofIntInfFromString (s, s->intInfInits[i].mlstr);
  }
  for (i = 0; i < s->vectorInitsLength; ++i) {
    dataBytes =
      s->vectorInits[i].bytesPerElement
      * s->vectorInits[i].numElements;
    total += align (GC_ARRAY_HEADER_SIZE
                    + ((dataBytes < OBJPTR_SIZE)
                       ? OBJPTR_SIZE
                       : dataBytes),
                    s->alignment);
  }
  return total;
}

void initIntInfs (GC_state s) {
  struct GC_intInfInit *inits;
  uint32_t i;
  const char *str;
  size_t bytes;
  bool neg;
  __mpz_struct resmpz;
  int ans;

  assert (isFrontierAligned (s, s->sharedFrontier));
  for (i = 0; i < s->intInfInitsLength; i++) {
    inits = &(s->intInfInits[i]);
    assert (inits->globalIndex < s->globalsLength);
    str = inits->mlstr;
    bytes = sizeofIntInfFromString (s, str);
    neg = *str == '~';
    if (neg)
      str++;
    initIntInfRes (s, &resmpz, bytes);
    ans = mpz_set_str (&resmpz, str, 10);
    assert (ans == 0);
    if (neg)
      resmpz._mp_size = - resmpz._mp_size;
    s->globals[inits->globalIndex] = finiIntInfRes (s, &resmpz, bytes);
    fprintf (stderr, "initIntInfs: HEADER = "FMTHDR"\n", getHeader ((pointer)s->globals[inits->globalIndex]));
    assert (0);
    exit (1);
  }
  assert (isFrontierAligned (s, s->sharedFrontier));
}

void initVectors (GC_state s) {
  struct GC_vectorInit *inits;
  pointer frontier;
  uint32_t i;

  assert (isFrontierAligned (s, s->frontier));
  inits = s->vectorInits;
  frontier = s->sharedFrontier;
  for (i = 0; i < s->vectorInitsLength; i++) {
    size_t bytesPerElement;
    size_t dataBytes;
    size_t objectSize;
    uint32_t typeIndex;

    bytesPerElement = inits[i].bytesPerElement;
    dataBytes = bytesPerElement * inits[i].numElements;
    objectSize = align (GC_ARRAY_HEADER_SIZE
                        + ((dataBytes < OBJPTR_SIZE)
                           ? OBJPTR_SIZE
                           : dataBytes),
                        s->alignment);
    assert (objectSize <= (size_t)(s->sharedHeap->start + s->sharedHeap->size - frontier));
    *((GC_arrayCounter*)(frontier)) = 0;
    frontier = frontier + GC_ARRAY_COUNTER_SIZE;
    *((GC_arrayLength*)(frontier)) = inits[i].numElements;
    frontier = frontier + GC_ARRAY_LENGTH_SIZE;
    switch (bytesPerElement) {
    case 1:
      typeIndex = WORD8_VECTOR_TYPE_INDEX;
      break;
    case 2:
      typeIndex = WORD16_VECTOR_TYPE_INDEX;
      break;
    case 4:
      typeIndex = WORD32_VECTOR_TYPE_INDEX;
      break;
    case 8:
      typeIndex = WORD64_VECTOR_TYPE_INDEX;
      break;
    default:
      die ("unknown bytes per element in vectorInit: %"PRIuMAX"",
           (uintmax_t)bytesPerElement);
    }
    *((GC_header*)(frontier)) = (buildHeaderFromTypeIndex (typeIndex) | LIFT_MASK);
    frontier = frontier + GC_HEADER_SIZE;
    s->globals[inits[i].globalIndex] = pointerToObjptr(frontier, s->sharedHeap->start);
    if (DEBUG_DETAILED)
      fprintf (stderr, "allocated vector at "FMTPTR"\n",
               (uintptr_t)(s->globals[inits[i].globalIndex]));
    memcpy (frontier, inits[i].bytes, dataBytes);
    frontier += objectSize - GC_ARRAY_HEADER_SIZE;
  }
  if (DEBUG_DETAILED)
    fprintf (stderr, "frontier after string allocation is "FMTPTR"\n",
             (uintptr_t)frontier);
  //GC_profileAllocInc (s, (size_t)(frontier - s->frontier));
  s->cumulativeStatistics->bytesLifted += (size_t)(frontier - s->sharedFrontier);
  assert (isFrontierAligned (s, frontier));
  s->sharedFrontier = frontier;
}

void initWorld (GC_state s) {
  uint32_t i;
  pointer start;
  GC_thread thread;
  size_t minSize;

  for (i = 0; i < s->globalsLength; ++i)
    s->globals[i] = BOGUS_OBJPTR;
  s->lastSharedMajorStatistics->bytesLive = sizeofInitialBytesLive (s);
  minSize = s->lastSharedMajorStatistics->bytesLive;
  if (s->controls->maxHeapShared != 0 && s->controls->maxHeapShared < minSize) {
    fprintf (stderr, "Unable to create shared heap with maxHeapShared %zu\n", minSize);
    exit (1);
  }
  size_t initSize = (s->controls->maxHeapShared != 0 && (s->controls->maxHeapShared < 131072))?
                     s->controls->maxHeapShared : 131072;
  createHeap (s, s->sharedHeap, sizeofHeapDesired (s, max(minSize,initSize), 0, SHARED_HEAP), minSize);

  //set up shared heap
  start = alignFrontier (s, s->sharedHeap->start);
  s->sharedStart = s->sharedFrontier = start;
  s->sharedLimitPlusSlop = s->sharedHeap->start + s->sharedHeap->size - GC_BONUS_SLOP;
  s->sharedLimit = s->sharedLimitPlusSlop - GC_HEAP_LIMIT_SLOP;
  initIntInfs (s);
  initVectors (s);
  s->sharedHeap->oldGenSize = s->sharedFrontier - s->sharedHeap->start;
  setGCStateCurrentSharedHeap (s, 0, 0, true);

  //set up local heap
  initSize = (s->controls->maxHeapLocal != 0 && (s->controls->maxHeapLocal < 65536))?
                    s->controls->maxHeapLocal:65536;
  createHeap (s, s->heap, sizeofHeapDesired (s, initSize, 0, LOCAL_HEAP), 0);
  setCardMapAndCrossMap (s);
  start = alignFrontier (s, s->heap->start);
  s->start = s->frontier = s->sessionStart = start;
  s->limitPlusSlop = s->heap->start + s->heap->size - GC_BONUS_SLOP;
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
  assert ((size_t)(s->frontier - start) <= s->lastMajorStatistics->bytesLive);
  s->heap->oldGenSize = s->frontier - s->heap->start;
  setGCStateCurrentLocalHeap (s, 0, 0);


  thread = newThread (s, sizeofStackInitialReserved (s));
  switchToThread (s, pointerToObjptr((pointer)thread - offsetofThread (s), s->heap->start));
}

void duplicateWorld (GC_state d, GC_state s) {
  GC_thread thread;
  pointer start;

  d->lastMajorStatistics->bytesLive = 0;
  //set up local heap
  d->heap = (GC_heap) malloc (sizeof (struct GC_heap));
  initHeap (d, d->heap, LOCAL_HEAP);
  size_t initSize = (s->controls->maxHeapLocal != 0 && (s->controls->maxHeapLocal < 65536))?
                    s->controls->maxHeapLocal:65536;
  createHeap (d, d->heap, sizeofHeapDesired (s, initSize, 0, LOCAL_HEAP), 0);
  start = alignFrontier (d, d->heap->start);
  d->start = d->frontier = d->sessionStart = start;
  d->limitPlusSlop = d->heap->start + d->heap->size - GC_BONUS_SLOP;
  d->limit = d->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
  d->heap->oldGenSize = d->frontier - d->heap->start;
  setGCStateCurrentLocalHeap (d, 0, 0);
  setCardMapAndCrossMap (d);

  //set up secondary local heap
  d->secondaryLocalHeap = (GC_heap) malloc (sizeof (struct GC_heap));
  initHeap (d, d->secondaryLocalHeap, LOCAL_HEAP);

  /* Use the original to allocate */
  thread = newThread (d, sizeofStackInitialReserved (d));

  d->cumulativeStatistics->maxHeapSize = d->heap->size;
  d->cumulativeStatistics->maxSharedHeapSize =
    s->cumulativeStatistics->maxSharedHeapSize;
  d->sharedHeap = s->sharedHeap;
  d->secondarySharedHeap = s->secondarySharedHeap;

  /* Allocation handled in setGCStateCurrentSharedHeap when called from initWorld */

  switchToThread (d, pointerToObjptr((pointer)thread - offsetofThread (d), d->heap->start));
}

